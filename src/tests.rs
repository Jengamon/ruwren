use super::{create_module, get_slot_checked, VMConfig};

struct Point {
    x: f64,
}

impl Point {
    fn x(&self, vm: &super::VM) {
        vm.ensure_slots(1);
        vm.set_slot_double(0, self.x);
    }

    fn set_x(&mut self, vm: &super::VM) {
        vm.ensure_slots(2);
        self.x = get_slot_checked!(vm => num 1);
    }
}

impl super::Class for Point {
    fn initialize(vm: &super::VM) -> Point {
        vm.ensure_slots(2);
        let x = get_slot_checked!(vm => num 1);
        Point {
            x,
        }
    }
}

struct Math;

impl super::Class for Math {
    fn initialize(_: &super::VM) -> Math { Math }
}

impl Math {
    fn add5(vm: &super::VM) {
        vm.ensure_slots(2);
        let i = get_slot_checked!(vm => num 1);
        vm.set_slot_double(0, i + 5.0);
    }

    fn pointy(vm: &super::VM) {
        vm.ensure_slots(2);
        let send = vm.set_slot_new_foreign("main", "RawPoint", Point {
            x: 345.7
        }, 0);
        if send.is_err() {
            panic!("Could not send RawPoint object");
        }
    }
}

create_module! {
    class("RawPoint") crate::tests::Point => point {
        instance(fn "x", 0) x,
        instance(fn "set_x", 1) set_x
    }

    class("Math") crate::tests::Math => math {
        static(fn "add5", 1) add5,
        static(fn "pointy", 0) pointy
    }

    module => main
}

#[test]
fn init_vm() {
    let _ = VMConfig::new().build();
}

#[test]
fn test_small_wren_program() {
    let vm = VMConfig::new().build();
    let interp = vm.interpret("main", "System.print(\"I am running in a VM!\")");
    println!("{:?}", interp);
    assert!(interp.is_ok());
}

#[test]
fn test_small_wren_program_call() {
    let vm = VMConfig::new().build();

    let source = vm.interpret("main", r"
    class GameEngine {
        static update(elapsedTime) {
            System.print(elapsedTime)
            return 16.45
        }
    }
    ");
    assert!(source.is_ok());

    vm.execute(|vm| {
        vm.ensure_slots(2);
        vm.get_variable("main", "GameEngine", 0);
        vm.set_slot_double(1, 32.2);
    });
    let interp = vm.call(super::FunctionSignature::new_function("update", 1));
    assert!(interp.is_ok());
    vm.execute(|vm| {
        assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
        assert_eq!(vm.get_slot_double(0), Some(16.45));
    });
}

#[test]
fn test_external_module() {
    let mut lib = super::ModuleLibrary::new();
    main::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    let source = vm.interpret("main", "
    class Math {
        foreign static add5(a)
    }

    foreign class RawPoint {
        construct new(x) {}

        foreign x()
        foreign set_x(val)
    }

    class Point {
        construct new(x) {
            _rp = RawPoint.new(x)
        }

        x { _rp.x() }
        x=(val) { _rp.set_x(val) }
    }

    class GameEngine {
        static update(elapsedTime) {
            System.print(elapsedTime)
            var p = Point.new(3)
            System.print(p.x)
            p.x = 10
            System.print(p.x)
            return Math.add5(16.45)
        }
    }
    ");
    println!("{:?}", source);
    assert!(source.is_ok());

    vm.execute(|vm| {
        vm.ensure_slots(2);
        vm.get_variable("main", "GameEngine", 0);
        vm.set_slot_double(1, 32.2);
    });
    let _ = vm.get_slot_handle(0);
    let interp = vm.call(super::FunctionSignature::new_function("update", 1));
    if let Err(e) = interp.clone() {
        eprintln!("{}", e);
    }
    assert!(interp.is_ok());
    vm.execute(|vm| {
        assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
        assert_eq!(vm.get_slot_double(0), Some(21.45));
    })
}

#[test]
fn test_script_module() {
    struct TestLoader;

    impl super::ModuleScriptLoader for TestLoader {
        fn load_script(&mut self, name: String) -> Option<String> {
            if name == "math" {
                Some("
                class Math {
                    static add5(val) {
                        return val + 5
                    }
                }
                ".into())
            } else {
                None
            }
        }
    }

    let vm = VMConfig::new().script_loader(TestLoader).build();
    let source = vm.interpret("main", "
    import \"math\" for Math

    class GameEngine {
        static update(elapsedTime) {
            System.print(elapsedTime)
            return Math.add5(16.45)
        }
    }
    ");
    assert!(source.is_ok());

    vm.execute(|vm| {
        vm.ensure_slots(2);
        vm.get_variable("main", "GameEngine", 0);
        vm.set_slot_double(1, 32.2);
    });
    let _ = vm.get_slot_handle(0);
    let interp = vm.call(super::FunctionSignature::new_function("update", 1));
    assert!(interp.is_ok());
    vm.execute(|vm| {
        assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
        assert_eq!(vm.get_slot_double(0), Some(21.45));
    });

}

#[test]
fn foreign_instance() {
    let mut lib = super::ModuleLibrary::new();
    main::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    let source = vm.interpret("main", "
    class Math {
        foreign static add5(a)
        foreign static pointy()
    }

    foreign class RawPoint {
        construct new(x) {}

        foreign x()
        foreign set_x(val)
    }

    class GameEngine {
        static update(elapsedTime) {
            System.print(elapsedTime)
            var p = Math.pointy()
            System.print(p.x())
            return Math.add5(16.45)
        }
    }
    ");
    println!("{:?}", source);
    assert!(source.is_ok());

    vm.execute(|vm| {
        vm.ensure_slots(2);
        vm.get_variable("main", "GameEngine", 0);
        vm.set_slot_double(1, 32.2);
    });

    let interp = vm.call(super::FunctionSignature::new_function("update", 1));
    let _ = vm.get_slot_handle(0);
    if let Err(e) = interp.clone() {
        eprintln!("{}", e);
    }
    assert!(interp.is_ok());
    vm.execute(|vm| {
        assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
        assert_eq!(vm.get_slot_double(0), Some(21.45));
    });
}