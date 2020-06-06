use criterion::{criterion_group, criterion_main, Criterion};
use ruwren::{Class, VM, VMConfig, ModuleLibrary, get_slot_checked, create_module};

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
    class("Fibonacci") crate::Fibonacci => fib {
        static(fn "fibonacci", 1) fibonacci
    }

    module => fibonacci
}

fn fibonacci_benchmark(c: &mut Criterion) {
    let mut lib = ModuleLibrary::new();
    fibonacci::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();

    vm.interpret("fibonacci", r#"
    class Fibonacci {
        foreign static fibonacci(n)
    }
    "#).unwrap();

    vm.interpret("main", r#"import "fibonacci" for Fibonacci"#).unwrap();

    c.bench_function("fib 20", |b| b.iter(|| {
        vm.interpret("main", r#"
        Fibonacci.fibonacci(20)
        "#).unwrap();
    }));
}

criterion_group!(benches, fibonacci_benchmark);
criterion_main!(benches);