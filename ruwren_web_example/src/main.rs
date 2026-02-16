use ruwren::{wren_impl, wren_module, ModuleLibrary, VMConfig, WrenObject};

#[derive(WrenObject, Default)]
struct Foo {
    bar: f64,
}

#[wren_impl]
impl Foo {
    /*
    you can also write out an allocator, if you
    don't want the base struct itself to implement Default

    #[wren_impl(allocator)]
    fn allocator() -> FooClass {
        FooClass {}
    }
    */

    #[wren_impl(constructor)]
    fn constructor(&self, bar: f64) -> Result<FooInstance, String> {
        Ok(FooInstance { bar })
    }

    #[wren_impl(instance)]
    fn instance(&self) -> f64 {
        self.bar
    }

    fn static_fn(&self, num: f64) -> f64 {
        num + 5.0
    }
}

wren_module! {
    mod foobar {
        pub crate::Foo;
    }
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
