use criterion::{criterion_group, criterion_main, Criterion};
use ruwren::{Class, VM, VMConfig, ModuleLibrary, FunctionSignature, get_slot_checked, create_module, send_foreign};

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
  y: u32
}

impl Class for Bonafide {
  fn initialize(vm: &VM) -> Bonafide {
    let x = get_slot_checked!(vm => num 1) as u32;
    let y = get_slot_checked!(vm => num 2) as u32;
    Bonafide {x, y}
  }
}

impl Bonafide {
  fn send_box(&self, vm: &VM) {
    send_foreign!(vm, "boxed", "DynBoxx", Box::new(self.clone()) as Box<dyn Boxx> => 0);
  }
}

impl Boxx for Bonafide {
  fn flipp(&self) -> u32 { self.x + self.y }
}

create_module! {
  class("Bonafide") crate::Bonafide => bonafide {
    instance(getter "box") send_box
  }

  class("DynBoxx") Box<dyn crate::Boxx> => boxx {
  }

  module => boxed
}

const BONAFIDE_MODULE_SRC: &'static str = include_str!("boxed/bonafide.wren");

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

    c.bench_function("box_embed", |b| {
        vm.execute(|vm| {
            vm.get_variable("main", "BoxedTest", 0);
        }); 

        let res = vm.call(FunctionSignature::new_getter("main"));

        if let Err(err) = res {
          eprintln!("{}", err);
        } else {
            b.iter(|| {
                vm.execute(|vm| {
                  let f = get_slot_checked!(vm => foreign Box<dyn crate::Boxx> => 0);
                  let _ = f.flipp();
                })
            });
        }
    });
}

criterion_group!(benches, fibonacci_benchmark, foreign_box_benchmark);
criterion_main!(benches);