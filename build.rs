fn main() {
    println!("cargo:rerun-if-changed=/src/syntax/parser.lalrpop");
    lalrpop::process_root().unwrap()
}
