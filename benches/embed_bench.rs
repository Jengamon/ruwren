use criterion::criterion_main;

const BONAFIDE_MODULE_SRC: &str = include_str!("boxed/bonafide.wren");

mod v2 {
    use super::BONAFIDE_MODULE_SRC;
    use criterion::{criterion_group, Criterion};
    use ruwren::{
        get_slot_checked, send_foreign, wren_impl, wren_module, FunctionSignature, ModuleLibrary,
        VMConfig, WrenObject, VM,
    };

    #[derive(WrenObject, Default)]
    struct Fibonacci;

    #[wren_impl]
    impl Fibonacci {
        #[allow(clippy::only_used_in_recursion)]
        fn fibonacci(&self, n: u64) -> u64 {
            match n {
                0 => 1,
                1 => 1,
                n => self.fibonacci(n - 1) + self.fibonacci(n - 2),
            }
        }
    }

    wren_module! {
        mod fibonacci {
            pub crate::v2::Fibonacci;
        }
    }

    fn fibonacci_benchmark(c: &mut Criterion) {
        let mut lib = ModuleLibrary::new();
        fibonacci::publish_module(&mut lib);
        let vm = VMConfig::new().library(&lib).build();

        vm.interpret(
            "fibonacci",
            r#"
    class Fibonacci {
        foreign static fibonacci(n)
    }
    "#,
        )
        .unwrap();

        vm.interpret("main", r#"import "fibonacci" for Fibonacci"#)
            .unwrap();

        c.bench_function("fib 20 v2", |b| {
            b.iter(|| {
                vm.interpret(
                    "main",
                    r#"
        Fibonacci.fibonacci(20)
        "#,
                )
                .unwrap();
            })
        });
    }

    trait Boxx {
        fn flipp(&self) -> u32;
    }

    impl ruwren::Class for Box<dyn Boxx> {
        fn initialize(_: &VM) -> Self {
            panic!("Can't initialize DynBoxx")
        }
    }

    impl ruwren::foreign_v2::WrenTo for Box<dyn Boxx> {
        fn to_vm(self, vm: &VM, slot: ruwren::SlotId, _scratch_start: ruwren::SlotId) {
            send_foreign!(vm, "boxed", "DynBoxx", self => slot);
        }
    }

    #[derive(Clone, WrenObject)]
    struct Bonafide {
        x: u32,
        y: u32,
    }

    #[wren_impl]
    impl Bonafide {
        #[wren_impl(allocator)]
        fn alloc() -> _ {
            BonafideClass {}
        }

        #[wren_impl(constructor)]
        fn constructor(&mut self, x: u32, y: u32) -> _ {
            BonafideInstance { x, y }
        }

        #[wren_impl(instance, getter)]
        fn boxed(&self) -> Box<dyn Boxx> {
            Box::<Bonafide>::new(self.into()) as Box<dyn Boxx>
        }
    }

    impl Boxx for Bonafide {
        fn flipp(&self) -> u32 {
            self.x + self.y
        }
    }

    mod v1 {
        use ruwren::create_module;
        create_module! {
            class("DynBoxx") Box<dyn crate::v2::Boxx> => boxx {
            }

            module => boxed
        }
    }

    #[allow(clippy::module_inception)]
    mod v2 {
        use ruwren::wren_module;
        wren_module! {
            pub mod boxed {
                pub crate::v2::Bonafide;
            }
        }
    }

    fn foreign_box_benchmark(c: &mut Criterion) {
        let mut lib = ModuleLibrary::new();
        v1::boxed::publish_module(&mut lib);
        v2::boxed::publish_module(&mut lib);
        let vm = VMConfig::new().library(&lib).build();
        vm.interpret("boxed", BONAFIDE_MODULE_SRC).unwrap(); // Should succeed

        let res = vm.interpret("main", include_str!("boxed/main.wren"));
        if let Err(err) = res {
            eprintln!("Load error: {}", err);
            return;
        }

        c.bench_function("box_embed v2", |b| {
            vm.execute(|vm| {
                vm.get_variable("main", "BoxedTest", 0);
            });

            let res = vm.call(FunctionSignature::new_getter("main"));

            if let Err(err) = res {
                eprintln!("{}", err);
            } else {
                b.iter(|| {
                    vm.execute(|vm| {
                        let f = get_slot_checked!(vm => foreign Box<dyn crate::v2::Boxx> => 0);
                        let _ = f.flipp();
                    })
                });
            }
        });
    }

    criterion_group!(benches_v2, fibonacci_benchmark, foreign_box_benchmark);
}

mod v1 {
    use super::BONAFIDE_MODULE_SRC;
    use criterion::{criterion_group, Criterion};
    use ruwren::{
        create_module, get_slot_checked, send_foreign, Class, FunctionSignature, ModuleLibrary,
        VMConfig, VM,
    };

    struct Fibonacci;

    impl Class for Fibonacci {
        fn initialize(_: &VM) -> Self {
            Fibonacci
        }
    }

    impl Fibonacci {
        fn fibonacci(vm: &VM) {
            let n = get_slot_checked!(vm => num 1);
            let r = match n as u64 {
                0 => 1,
                1 => 1,
                n => {
                    vm.set_slot_double(1, (n - 1) as f64);
                    Fibonacci::fibonacci(vm);
                    let l = get_slot_checked!(vm => num 0) as u64;
                    vm.set_slot_double(1, (n - 2) as f64);
                    Fibonacci::fibonacci(vm);
                    let r = get_slot_checked!(vm => num 0) as u64;
                    l + r
                }
            };
            vm.set_slot_double(0, r as f64);
        }
    }

    create_module! {
        class("Fibonacci") crate::v1::Fibonacci => fib {
            static(fn "fibonacci", 1) fibonacci
        }

        module => fibonacci
    }

    fn fibonacci_benchmark(c: &mut Criterion) {
        let mut lib = ModuleLibrary::new();
        fibonacci::publish_module(&mut lib);
        let vm = VMConfig::new().library(&lib).build();

        vm.interpret(
            "fibonacci",
            r#"
    class Fibonacci {
        foreign static fibonacci(n)
    }
    "#,
        )
        .unwrap();

        vm.interpret("main", r#"import "fibonacci" for Fibonacci"#)
            .unwrap();

        c.bench_function("fib 20 v1", |b| {
            b.iter(|| {
                vm.interpret(
                    "main",
                    r#"
        Fibonacci.fibonacci(20)
        "#,
                )
                .unwrap();
            })
        });
    }

    trait Boxx {
        fn flipp(&self) -> u32;
    }

    impl Class for Box<dyn Boxx> {
        fn initialize(_: &VM) -> Self {
            panic!("Can't initialize DynBoxx")
        }
    }

    #[derive(Clone)]
    struct Bonafide {
        x: u32,
        y: u32,
    }

    impl Class for Bonafide {
        fn initialize(vm: &VM) -> Bonafide {
            let x = get_slot_checked!(vm => num 1) as u32;
            let y = get_slot_checked!(vm => num 2) as u32;
            Bonafide { x, y }
        }
    }

    impl Bonafide {
        fn send_box(&self, vm: &VM) {
            send_foreign!(vm, "boxed", "DynBoxx", Box::new(self.clone()) as Box<dyn Boxx> => 0);
        }
    }

    impl Boxx for Bonafide {
        fn flipp(&self) -> u32 {
            self.x + self.y
        }
    }

    create_module! {
      class("Bonafide") crate::v1::Bonafide => bonafide {
        instance(getter "boxed") send_box
      }

      class("DynBoxx") Box<dyn crate::v1::Boxx> => boxx {
      }

      module => boxed
    }

    fn foreign_box_benchmark(c: &mut Criterion) {
        let mut lib = ModuleLibrary::new();
        boxed::publish_module(&mut lib);
        let vm = VMConfig::new().library(&lib).build();
        vm.interpret("boxed", BONAFIDE_MODULE_SRC).unwrap(); // Should succeed

        let res = vm.interpret("main", include_str!("boxed/main.wren"));
        if let Err(err) = res {
            eprintln!("Load error: {}", err);
            return;
        }

        c.bench_function("box_embed v1", |b| {
            vm.execute(|vm| {
                vm.get_variable("main", "BoxedTest", 0);
            });

            let res = vm.call(FunctionSignature::new_getter("main"));

            if let Err(err) = res {
                eprintln!("{}", err);
            } else {
                b.iter(|| {
                    vm.execute(|vm| {
                        let f = get_slot_checked!(vm => foreign Box<dyn crate::v1::Boxx> => 0);
                        let _ = f.flipp();
                    })
                });
            }
        });
    }

    criterion_group!(benches_v1, fibonacci_benchmark, foreign_box_benchmark);
}
criterion_main!(v1::benches_v1, v2::benches_v2);
