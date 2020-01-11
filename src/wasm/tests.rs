use once_cell::sync::Lazy;
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

    fn memory(&self) -> MemoryRef {
        self.instance
            .export_by_name("memory")
            .unwrap()
            .as_memory()
            .cloned()
            .unwrap()
    }

    fn memory_alloc(&self, size: i32) -> Result<i32, Error> {
        let result = self.instance.invoke_export(
            "memory_alloc",
            &[RuntimeValue::I32(size)],
            &mut NopExternals,
        )?;
        Ok(result.unwrap().try_into().unwrap())
    }

    fn memory_copy(&self, src: i32, dst: i32, len: i32) -> Result<(), Error> {
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
}

mod memory_alloc {
    use wasmi::memory_units::Pages;
    use wasmi::LINEAR_MEMORY_PAGE_SIZE;

    use super::Runtime;

    #[test]
    fn bump() {
        let rt = Runtime::instantiate();

        assert_eq!(rt.memory_alloc(5).unwrap(), 0);
        assert_eq!(rt.memory_alloc(10).unwrap(), 5);
        assert_eq!(rt.memory_alloc(6).unwrap(), 15);
    }

    #[test]
    fn grow() {
        let rt = Runtime::instantiate();
        let mem = rt.memory();

        assert_eq!(mem.current_size(), Pages(1));
        rt.memory_alloc(20).unwrap();
        assert_eq!(mem.current_size(), Pages(1));
        rt.memory_alloc(LINEAR_MEMORY_PAGE_SIZE.0 as i32 * 1)
            .unwrap();
        assert_eq!(mem.current_size(), Pages(2));
        rt.memory_alloc(40).unwrap();
        assert_eq!(mem.current_size(), Pages(2));
        rt.memory_alloc(LINEAR_MEMORY_PAGE_SIZE.0 as i32 * 2)
            .unwrap();
        assert_eq!(mem.current_size(), Pages(4));
    }
}

mod memory_copy {
    use super::Runtime;

    #[test]
    fn string() {
        let rt = Runtime::instantiate();
        let mem = rt.memory();

        const HELLO: &'static str = "Hello, world!";
        let src = rt.memory_alloc(HELLO.len() as i32).unwrap();
        let dst = rt.memory_alloc(HELLO.len() as i32).unwrap();
        mem.set(src as u32, HELLO.as_bytes()).unwrap();

        rt.memory_copy(src, dst, HELLO.len() as i32).unwrap();

        assert_eq!(mem.get(dst as u32, HELLO.len()).unwrap(), HELLO.as_bytes());
    }

    #[test]
    fn empty() {
        let rt = Runtime::instantiate();

        let src = rt.memory_alloc(1).unwrap();
        let dst = rt.memory_alloc(2).unwrap();

        rt.memory_copy(src, dst, 0).unwrap();
    }
}
