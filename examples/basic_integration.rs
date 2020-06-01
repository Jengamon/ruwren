use ruwren::{VM, Class, VMConfig, ModuleLibrary, create_module, get_slot_checked, send_foreign};

struct Vector {
    x: f64,
    y: f64
}

impl Class for Vector {
    fn initialize(_: &VM) -> Self {
        panic!("Cannot initialize from Wren code");
    }
}

impl Drop for Vector {
    fn drop(&mut self) {
        println!("Dropping Vector {} {}", self.x, self.y)
    }
}

impl Vector {
    fn x(&self, vm: &VM) {
        vm.set_slot_double(0, self.x);
    }

    fn y(&self, vm: &VM) {
        vm.set_slot_double(0, self.y);
    }

    fn set_x(&mut self, vm: &VM) {
        self.x = get_slot_checked!(vm => num 1);
    }

    fn set_y(&mut self, vm: &VM) {
        self.y = get_slot_checked!(vm => num 1);
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
        send_foreign!(vm, "maths", "Vector", Vector { x, y } => 0);
    }
}

create_module!(
    class("Vector") crate::Vector => vector {
        instance(getter "x") x,
        instance(getter "y") y,
        instance(setter "x") set_x,
        instance(setter "y") set_y
    }

    class("Math") crate::Math => math {
        static(fn "new_vector", 2) new_vector
    }

    module => maths
);

static MATHS_MODULE_SRC: &'static str = include_str!("basic_integration/maths.wren");

fn main() {
    let mut lib = ModuleLibrary::new();
    maths::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    vm.interpret("maths", MATHS_MODULE_SRC).unwrap(); // Should succeed

    let res = vm.interpret("main", include_str!("basic_integration/main.wren"));

    if let Err(err) = res {
        eprintln!("{}", err);
    }
}