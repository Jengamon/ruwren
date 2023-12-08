use crate::ModuleScriptLoader;
use std::path::{Path, PathBuf};

pub struct NullLoader;
impl ModuleScriptLoader for NullLoader {
    fn load_script(&mut self, _: String) -> Option<String> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct BasicFileLoader {
    base_dir: PathBuf,
}

impl Default for BasicFileLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl BasicFileLoader {
    pub fn new() -> BasicFileLoader {
        BasicFileLoader {
            base_dir: ".".into(),
        }
    }
    pub fn base_dir<P: AsRef<Path>>(mut self, base_dir: P) -> Self {
        self.base_dir = base_dir.as_ref().into();
        self
    }
}
/// Enable to load wren scripts from a base directory
impl ModuleScriptLoader for BasicFileLoader {
    fn load_script(&mut self, module: String) -> Option<String> {
        use std::fs::File;
        let module_path = self.base_dir.join(module).with_extension("wren");
        let mut contents = String::new();
        match File::open(module_path) {
            Ok(mut file) => {
                use std::io::Read;
                match file.read_to_string(&mut contents) {
                    Ok(_) => Some(contents),
                    Err(file) => {
                        eprintln!("failed to read file {:?}", file);
                        None
                    }
                }
            }
            Err(file) => {
                eprintln!("failed to open file {:?}", file);
                None
            }
        }
    }
}
