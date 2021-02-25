use ruwren::{create_module, Class, FunctionSignature, ModuleLibrary, VMConfig, VM};
use std::time::{Instant, SystemTime};

struct Counter {
    count: i32,
}

impl Class for Counter {
    fn initialize(_: &VM) -> Counter {
        Counter { count: 0 }
    }
}

impl Counter {
    fn ping(&mut self, vm: &VM) {
        vm.set_slot_double(0, self.count as f64);
        self.count += 1;
    }

    fn time(vm: &VM) {
        let now = SystemTime::now();
        let dur = now.duration_since(SystemTime::UNIX_EPOCH);
        match dur {
            Err(_) => vm.set_slot_null(0),
            Ok(dur) => vm.set_slot_double(0, dur.as_secs_f64()),
        };
    }
}

create_module! {
    class("Counter") crate::Counter => counter {
        static(getter "time") time,
        instance(getter "ping") ping
    }

    module => main
}

fn main() {
    let mut lib = ModuleLibrary::new();
    main::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();

    vm.interpret("main", include_str!("handles/main.wren"))
        .unwrap(); // Should succeed

    vm.execute(|vm| {
        vm.ensure_slots(1);
        vm.get_variable("main", "Main", 0);
    });
    let main_class = vm.get_slot_handle(0);
    let main_function = vm.make_call_handle(FunctionSignature::new_function("main", 0));

    let start = Instant::now();
    for i in 0..10 {
        println!("=== ITERATION {} ===", i);
        vm.set_slot_handle(0, &main_class);
        let res = vm.call_handle(&main_function);
        if let Err(err) = res {
            eprintln!("{}", err);
            break;
        }
    }
    println!("Finished in {:?}", Instant::now() - start);
}
