use ruwren::{VMConfig, ModuleScriptLoader};

struct TestLoader;

impl ModuleScriptLoader for TestLoader {
	fn load_script(&mut self, module: String) -> Option<String> {
		if module == "test/a" {
			Some(include_str!("relative_imports/a.wren").into())
		} else if module == "test" {
			Some(include_str!("relative_imports/test.wren").into())
		} else {
			None
		}
	}
}

fn main() {
	let vm = VMConfig::new()
		.enable_relative_import(true)
		.script_loader(TestLoader)
		.build();

	let main_script = include_str!("relative_imports/main.wren");
	match vm.interpret("main", main_script) {
		Ok(_) => {},
		Err(e) => {
			eprintln!("{}", e);
			panic!("Unexpected error!");
		}
	}
}