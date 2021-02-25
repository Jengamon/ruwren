use ruwren::{BasicFileLoader, VMConfig};

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
