use ruwren::{BasicFileLoader, VMConfig};

// To run this on wasmtime, it must be given permission to access the script directory
// for wasmtime, pass in --dir=examples/relative_imports (if running from the root directory)
// using the just file, this would be just wasm-example relative_imports --dir=examples/relative_imports

fn main() {
    let script_loader = BasicFileLoader::new().base_dir("examples/relative_imports");

    let vm = VMConfig::new()
        .enable_relative_import(true)
        .script_loader(script_loader)
        .build();

    let main_script = include_str!("relative_imports/main.wren");
    match vm.interpret("main", main_script) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            panic!("Unexpected error!");
        }
    }
}
