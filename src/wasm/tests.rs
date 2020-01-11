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

    fn alloc(&self, size: i32) -> Result<i32, Error> {
        Ok(self
            .instance
            .invoke_export("alloc", &[RuntimeValue::I32(size)], &mut NopExternals)?
            .unwrap()
            .try_into()
            .unwrap())
    }
}

mod alloc {
    use wasmi::memory_units::Pages;
    use wasmi::LINEAR_MEMORY_PAGE_SIZE;

    use super::Runtime;

    #[test]
    fn bump() {
        let rt = Runtime::instantiate();

        assert_eq!(rt.alloc(5).unwrap(), 0);
        assert_eq!(rt.alloc(10).unwrap(), 5);
        assert_eq!(rt.alloc(6).unwrap(), 15);
    }

    #[test]
    fn grow() {
        let rt = Runtime::instantiate();
        let mem = rt.memory();

        assert_eq!(mem.current_size(), Pages(1));
        rt.alloc(20).unwrap();
        assert_eq!(mem.current_size(), Pages(1));
        rt.alloc(LINEAR_MEMORY_PAGE_SIZE.0 as i32 * 1).unwrap();
        assert_eq!(mem.current_size(), Pages(2));
        rt.alloc(40).unwrap();
        assert_eq!(mem.current_size(), Pages(2));
        rt.alloc(LINEAR_MEMORY_PAGE_SIZE.0 as i32 * 2).unwrap();
        assert_eq!(mem.current_size(), Pages(4));
    }
}
