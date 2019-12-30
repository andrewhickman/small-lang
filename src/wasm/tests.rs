use wasmer_runtime::error::RuntimeError;
use wasmer_runtime::{imports, instantiate, Instance, Memory};

const RT_WASM: &'static [u8] = include_bytes!(concat!(env!("OUT_DIR"), "/rt.wasm"));

struct Runtime {
    instance: Instance,
}

impl Runtime {
    fn instantiate() -> Self {
        Runtime {
            instance: instantiate(RT_WASM, &imports![]).unwrap(),
        }
    }

    fn memory(&self) -> &Memory {
        self.instance.context().memory(0)
    }

    fn alloc(&self, size: i32) -> Result<i32, RuntimeError> {
        self.instance.func::<i32, i32>("alloc").unwrap().call(size)
    }
}

mod alloc {
    use wasmer_runtime::units::Pages;

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
        const PAGE_SIZE: i32 = 65_536;

        let rt = Runtime::instantiate();
        let mem = rt.memory();

        assert_eq!(mem.size(), Pages(1));
        rt.alloc(20).unwrap();
        assert_eq!(mem.size(), Pages(1));
        rt.alloc(PAGE_SIZE * 1).unwrap();
        assert_eq!(mem.size(), Pages(2));
        rt.alloc(40).unwrap();
        assert_eq!(mem.size(), Pages(2));
        rt.alloc(PAGE_SIZE * 2).unwrap();
        assert_eq!(mem.size(), Pages(4));
    }
}
