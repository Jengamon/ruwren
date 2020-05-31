use ruwren::{VM, Class, PrintlnPrinter, NullLoader, ModuleLibrary, create_class_objects, SlotType, Executor};

struct Vector {
    x: f64,
    y: f64
}

impl Class for Vector {
    fn initialize(vm: &VM) -> Self {
        panic!("Cannot initialize from Wren code");
    }
}

impl Vector {
    fn x(&self, vm: &VM) {
        vm.ensure_slots(1);
        vm.set_slot_double(0, self.x);
    }

    fn y(&self, vm: &VM) {
        vm.ensure_slots(1);
        vm.set_slot_double(0, self.y);
    }

    fn set_x(&mut self, vm: &VM) {
        vm.ensure_slots(2);
        if vm.get_slot_type(1) != SlotType::Num { panic!("x can only be set to <num>") }
        self.x = vm.get_slot_double(1);
    }

    fn set_y(&mut self, vm: &VM) {
        vm.ensure_slots(2);
        if vm.get_slot_type(1) != SlotType::Num { panic!("y can only be set to <num>") }
        self.y = vm.get_slot_double(1);
    }
}

struct Math;

impl Class for Math {
    fn initialize(vm: &VM) -> Self {
        panic!("Math is a purely static class");
    }
}

impl Math {
    fn new_vector(vm: &VM) {
        vm.ensure_slots(3);
        if vm.get_slot_type(1) != SlotType::Num { panic!("a vector's x can only be <num>") }
        if vm.get_slot_type(2) != SlotType::Num { panic!("a vector's y can only by <num>") }
        let x = vm.get_slot_double(1);
        let y = vm.get_slot_double(2);
        if vm.set_slot_new_foreign("maths", "Vector", Vector {
            x, y
        }, 0).is_none() {
            panic!("Unable to send vector")
        }
    }
}

create_class_objects!(
    class("Vector") crate::Vector => vector {
        instance("x()") x,
        instance("y()") y,
        instance("set_x(_)") set_x,
        instance("set_y(_)") set_y
    }

    class("Math") crate::Math => math {
        static("new_vector(_,_)") new_vector
    }

    module => maths
);

static MATHS_MODULE_SRC: &'static str = "
foreign class Vector {
    construct invalid() {}
    foreign x()
    foreign y()
    foreign set_x(x)
    foreign set_y(y)
}

class Math {
    foreign static new_vector(x, y)
}
";

fn main() {
    let mut lib = ModuleLibrary::new();
    maths::publish_module(&mut lib);
    let vm = VM::new(PrintlnPrinter, NullLoader, Some(&lib));
    vm.execute(|vm| {
        vm.interpret("maths", MATHS_MODULE_SRC).unwrap(); // Should succeed
    });

    vm.execute(|vm| {
        let res = vm.interpret("main", "
        import \"maths\" for Math, Vector
        var poke_vector = Fiber.new {
            Vector.invalid()
        }
        System.print(\"Hello World\")
        System.print(poke_vector.try())
        var vector = Math.new_vector(3, 4)
        System.print(vector)
        System.print(vector.x())
        System.print(vector.y())
        vector.set_x(10.2)
        vector.set_y(vector.x() * 2)
        System.print(vector.x())
        System.print(vector.y())
        ");

        if let Err(err) = res {
            eprintln!("{}", err);
        }
    })
}