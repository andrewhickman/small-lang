use once_cell::sync::Lazy;
use wasmi::memory_units::Pages;
use wasmi::LINEAR_MEMORY_PAGE_SIZE;
use wasmi::{
    Error, ImportsBuilder, MemoryRef, Module, ModuleInstance, ModuleRef, NopExternals, RuntimeValue,
};

const RT_WASM: &'static [u8] = include_bytes!(concat!(env!("OUT_DIR"), "/rt.wasm"));
static RT_MODULE: Lazy<Module> = Lazy::new(|| Module::from_buffer(RT_WASM).unwrap());

struct Runtime {
    instance: ModuleRef,
}

impl Runtime {
    fn instantiate() -> Self {
        let instance = ModuleInstance::new(&RT_MODULE, &ImportsBuilder::default())
            .unwrap()
            .run_start(&mut NopExternals)
            .unwrap();
        Runtime { instance }
    }

    fn instantiate_child(&self, wat: &str) -> ModuleRef {
        let wasm = wabt::wat2wasm(wat).unwrap();
        ModuleInstance::new(
            &Module::from_buffer(wasm).unwrap(),
            &ImportsBuilder::new().with_resolver("rt", &self.instance),
        )
        .unwrap()
        .run_start(&mut NopExternals)
        .unwrap()
    }

    fn memory(&self) -> MemoryRef {
        self.instance
            .export_by_name("memory")
            .unwrap()
            .as_memory()
            .cloned()
            .unwrap()
    }

    fn call_memory_alloc(&self, size: i32) -> Result<i32, Error> {
        let result = self.instance.invoke_export(
            "memory_alloc",
            &[RuntimeValue::I32(size)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_memory_copy(&self, src: i32, dst: i32, len: i32) -> Result<(), Error> {
        let result = self.instance.invoke_export(
            "memory_copy",
            &[
                RuntimeValue::I32(src),
                RuntimeValue::I32(dst),
                RuntimeValue::I32(len),
            ],
            &mut NopExternals,
        )?;
        assert!(result.is_none());
        Ok(())
    }

    fn call_pair_new(&self, ptr: i32, len: i32) -> Result<i64, Error> {
        let result = self.instance.invoke_export(
            "pair_new",
            &[RuntimeValue::I32(ptr), RuntimeValue::I32(len)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_pair_first(&self, val: i64) -> Result<i32, Error> {
        let result = self.instance.invoke_export(
            "pair_first",
            &[RuntimeValue::I64(val)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_pair_second(&self, val: i64) -> Result<i32, Error> {
        let result = self.instance.invoke_export(
            "pair_second",
            &[RuntimeValue::I64(val)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_string_new(&self, ptr: i32, len: i32) -> Result<i64, Error> {
        let result = self.instance.invoke_export(
            "string_new",
            &[RuntimeValue::I32(ptr), RuntimeValue::I32(len)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_string_add(&self, lhs: i64, rhs: i64) -> Result<i64, Error> {
        let result = self.instance.invoke_export(
            "string_add",
            &[RuntimeValue::I64(lhs), RuntimeValue::I64(rhs)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_record_new(&self, len: i32, phf: i32) -> Result<i64, Error> {
        let result = self.instance.invoke_export(
            "record_new",
            &[RuntimeValue::I32(len), RuntimeValue::I32(phf)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn call_record_set(&self, record: i64, label: i32, value: i64) -> Result<(), Error> {
        let result = self.instance.invoke_export(
            "record_set",
            &[
                RuntimeValue::I64(record),
                RuntimeValue::I32(label),
                RuntimeValue::I64(value),
            ],
            &mut NopExternals,
        )?;
        assert!(result.is_none());
        Ok(())
    }

    fn call_record_get(&self, record: i64, label: i32) -> Result<i64, Error> {
        let result = self.instance.invoke_export(
            "record_get",
            &[RuntimeValue::I64(record), RuntimeValue::I32(label)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn string_new(&self, s: &str) -> i64 {
        let len = s.len() as i32;
        let ptr = self.call_memory_alloc(len).unwrap();

        self.memory().set(ptr as u32, s.as_bytes()).unwrap();
        self.call_string_new(ptr, len).unwrap()
    }

    fn string_get(&self, val: i64) -> String {
        let ptr = self.call_pair_first(val).unwrap();
        let len = self.call_pair_second(val).unwrap();
        String::from_utf8(self.memory().get(ptr as u32, len as usize).unwrap()).unwrap()
    }
}

#[test]
fn memory_alloc_bump() {
    let rt = Runtime::instantiate();

    assert_eq!(rt.call_memory_alloc(5).unwrap(), 0);
    assert_eq!(rt.call_memory_alloc(10).unwrap(), 5);
    assert_eq!(rt.call_memory_alloc(6).unwrap(), 15);
}

#[test]
fn memory_alloc_grow() {
    let rt = Runtime::instantiate();
    let mem = rt.memory();

    assert_eq!(mem.current_size(), Pages(1));
    rt.call_memory_alloc(20).unwrap();
    assert_eq!(mem.current_size(), Pages(1));
    rt.call_memory_alloc(LINEAR_MEMORY_PAGE_SIZE.0 as i32 * 1)
        .unwrap();
    assert_eq!(mem.current_size(), Pages(2));
    rt.call_memory_alloc(40).unwrap();
    assert_eq!(mem.current_size(), Pages(2));
    rt.call_memory_alloc(LINEAR_MEMORY_PAGE_SIZE.0 as i32 * 2)
        .unwrap();
    assert_eq!(mem.current_size(), Pages(4));
}

#[test]
fn memory_copy_string() {
    let rt = Runtime::instantiate();
    let mem = rt.memory();

    const HELLO: &'static str = "Hello, world!";
    let src = rt.call_memory_alloc(HELLO.len() as i32).unwrap();
    let dst = rt.call_memory_alloc(HELLO.len() as i32).unwrap();
    mem.set(src as u32, HELLO.as_bytes()).unwrap();

    rt.call_memory_copy(src, dst, HELLO.len() as i32).unwrap();

    assert_eq!(mem.get(dst as u32, HELLO.len()).unwrap(), HELLO.as_bytes());
}

#[test]
fn memory_copy_empty() {
    let rt = Runtime::instantiate();

    let src = rt.call_memory_alloc(1).unwrap();
    let dst = rt.call_memory_alloc(2).unwrap();

    rt.call_memory_copy(src, dst, 0).unwrap();
}

#[test]
fn pair_new() {
    let rt = Runtime::instantiate();

    assert_eq!(
        rt.call_pair_new(0x0123_4567u32 as i32, 0x89AB_CDEFu32 as i32)
            .unwrap(),
        0x0123_4567_89AB_CDEF as i64
    );
}

#[test]
fn pair_first() {
    let rt = Runtime::instantiate();

    assert_eq!(
        rt.call_pair_first(0x0123_4567_89AB_CDEF as i64).unwrap(),
        0x0123_4567u32 as i32
    );
}

#[test]
fn call_pair_second() {
    let rt = Runtime::instantiate();

    assert_eq!(
        rt.call_pair_second(0x0123_4567_89AB_CDEF as i64).unwrap(),
        0x89AB_CDEFu32 as i32
    );
}

#[test]
fn string_add() {
    let rt = Runtime::instantiate();

    let lhs = rt.string_new("Hello, ");
    let rhs = rt.string_new("world!");
    let add = rt.call_string_add(lhs, rhs).unwrap();
    assert_eq!(rt.string_get(add), "Hello, world!");
}

#[test]
fn record() {
    let rt = Runtime::instantiate();
    let _src = rt.instantiate_child(
        r#"(module
          (import "rt" "table" (table $table 1 funcref))
          (elem $table (offset i32.const 0) $id)
          (func $id (param $label i32) (result i32)
            (local.get $label)
          )
        )"#,
    );

    println!(
        "{:?}",
        rt.instance
            .export_by_name("table")
            .unwrap()
            .as_table()
            .unwrap()
    );

    let record = rt.call_record_new(3, 0).unwrap();

    rt.call_record_set(record, 3, 42).unwrap();
    assert_eq!(rt.call_record_get(record, 3).unwrap(), 42);
}
