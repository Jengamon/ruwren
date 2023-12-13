use ruwren::{create_module, get_slot_checked, Class, ModuleLibrary, VMConfig, VM};
struct Foo {
    bar: f64,
}

impl Class for Foo {
    fn initialize(vm: &VM) -> Self {
        let bar = get_slot_checked!(vm => num 1);
        Foo { bar }
    }
}

impl Foo {
    fn instance(&self, vm: &VM) {
        vm.set_slot_double(0, self.bar);
    }

    fn static_fn(vm: &VM) {
        let num = get_slot_checked!(vm => num 1);
        vm.set_slot_double(0, num + 5.0)
    }
}

create_module! {
    class("Foo") crate::Foo => foo {
        instance(fn "instance", 0) instance,
        static(fn "static_fn", 1) static_fn
    }

    module => foobar
}

fn main() {
    let mut lib = ModuleLibrary::new();
    foobar::publish_module(&mut lib);

    let vm = VMConfig::new().library(&lib).build();
    vm.interpret(
        "foobar",
        r##"
    foreign class Foo {
        construct new(bar) {}
        foreign instance()
        foreign static static_fn(num)
    }
    "##,
    )
    .unwrap();

    // You could now write Wren code like:

    vm.interpret(
        "main",
        r##"
    import "foobar" for Foo
    var f = Foo.new(4)
    System.print(Foo.static_fn(f.instance()))
    "##,
    )
    .unwrap();

    // This should print "9".
}
