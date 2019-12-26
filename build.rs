use std::error::Error;
use std::path::PathBuf;
use std::{env, fs};

fn main() {
    build_parser().unwrap();
    build_wasm().unwrap();
}

fn build_parser() -> Result<(), Box<dyn Error>> {
    println!("cargo:rerun-if-changed=src/syntax/parser.lalrpop");
    lalrpop::process_root().unwrap();
    Ok(())
}

fn build_wasm() -> Result<(), Box<dyn Error>> {
    let src = "src/wasm/rt.wat";
    let dst = PathBuf::from(env::var_os("OUT_DIR").ok_or("`OUT_DIR` not set")?).join("rt.wasm");

    println!("cargo:rerun-if-changed={}", src);
    let wat = fs::read(src)?;
    let wasm = wabt::wat2wasm(wat)?;
    fs::write(dst, wasm)?;
    Ok(())
}
