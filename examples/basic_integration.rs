use ruwren::{
    get_slot_checked, send_foreign, wren_impl, Class, ModuleLibrary, VMConfig, WrenObject, VM,
};

#[derive(WrenObject, Debug, Clone)]
struct Vector {
    x: f64,
    y: f64,
}

impl Drop for VectorInstance {
    fn drop(&mut self) {
        println!("Dropping Vector {} {}", self.x, self.y)
    }
}

#[wren_impl]
impl Vector {
    #[wren_impl(allocator)]
    fn alloc() {
        VectorClass {}
    }

    #[wren_impl(constructor)]
    fn construct(_class: &VectorClass) -> _ {
        panic!("Cannot initialize from Wren code");
    }

    #[wren_impl(instance)]
    fn copy(&self) -> Vector {
        self.into()
    }

    #[wren_impl(instance, getter)]
    fn x(&self) -> f64 {
        self.x
    }

    #[wren_impl(instance, getter)]
    fn y(&self) -> f64 {
        self.y
    }

    #[wren_impl(instance, setter)]
    fn x(&mut self, x: f64) {
        self.x = x;
    }

    #[wren_impl(instance, setter)]
    fn y(&mut self, y: f64) {
        self.y = y;
    }

    #[wren_impl(object(vecs))]
    fn read(&self, vecs: Vec<Option<Vector>>) {
        dbg!(&vecs);
        let vecs: Vec<Vector> = vecs.into_iter().flatten().collect();
        dbg!(vecs);
    }
}

struct Math;

impl Class for Math {
    fn initialize(_: &VM) -> Self {
        panic!("Math is a purely static class");
    }
}

impl Math {
    fn new_vector(vm: &VM) {
        let x = get_slot_checked!(vm => num 1);
        let y = get_slot_checked!(vm => num 2);
        send_foreign!(vm, "maths", "Vector", VectorInstance { x, y } => 0);
    }
}

mod v1 {
    use ruwren::create_module;
    create_module!(
        class("Math") crate::Math => math {
            static(fn "new_vector", 2) new_vector
        }

        module => maths
    );
}

mod v2 {
    use ruwren::wren_module;
    wren_module! {
        pub mod maths {
            pub crate::Vector;
        }
    }
}

static MATHS_MODULE_SRC: &str = include_str!("basic_integration/maths.wren");

fn main() {
    let mut lib = ModuleLibrary::new();
    v1::maths::publish_module(&mut lib);
    v2::maths::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    vm.interpret("maths", MATHS_MODULE_SRC).unwrap(); // Should succeed

    let res = vm.interpret("main", include_str!("basic_integration/main.wren"));

    if let Err(err) = res {
        eprintln!("{}", err);
    }
}
