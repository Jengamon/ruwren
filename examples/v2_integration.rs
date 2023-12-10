use ruwren::{wren_impl, wren_module, ModuleLibrary, VMConfig, WrenObject};

#[derive(WrenObject)]
struct Unit;

#[derive(WrenObject)]
struct NewType(u8);

#[derive(WrenObject)]
struct Tuple(u8, u8, #[wren(static_member)] u8, u8);

#[derive(WrenObject)]
struct Foo {
    bar: f64,
    #[wren(static_member)]
    sbar: i32,
}

#[wren_impl]
impl Foo {
    #[wren_impl(allocator)]
    // The return type of any method marked #[constructor] *must* be Self.
    // Only one method may be marked constructor for any #[wren_impl]
    fn new() -> FooClass {
        FooClass { sbar: 0 }
    }

    // Basically, self changes statically
    // constructor -> FooClass
    // any method not marked "instance" -> FooClass
    // any method marked "instance" -> FooWrapper
    // getter means that other than self, method cannot accept any arguments
    // setter means that other than self, method accept 1 argument, and return must be ()

    #[wren_impl(constructor)]
    fn construct(&mut self, bar: f64) -> FooInstance {
        FooInstance { bar }
    }

    // This is only given a FooClass
    #[wren_impl(object(ifoo))]
    fn static_fn(&mut self, num: i32, ifoo: Option<Foo>) -> i32 {
        if let Some(foo) = ifoo {
            eprintln!(
                "got a Foo instance, is self-consistent? {}",
                foo.sbar == self.sbar
            )
        } else {
            eprintln!("no Foo instance...");
        }
        self.sbar += num;
        self.sbar
    }

    // a static getter
    #[wren_impl(getter)]
    fn sbar(&mut self) -> i32 {
        self.sbar
    }

    // an instance getter
    #[wren_impl(instance, setter)]
    fn bar(&mut self, nbar: f64) {
        *self.bar = nbar;
    }

    #[wren_impl(instance)]
    // This is given a full Foo(Wrapper), not FooClass or FooInstance
    fn instance(&self) -> f64 {
        *self.bar
    }
}

wren_module! {
    mod foobar {
        pub crate::Foo;
    }
}

const FOOBAR_SRC: &'static str = include_str!("v2_integration/foobar.wren");

fn main() {
    let mut lib = ModuleLibrary::new();
    foobar::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    vm.interpret("foobar", FOOBAR_SRC).unwrap();

    let res = vm.interpret("main", include_str!("v2_integration/main.wren"));

    if let Err(err) = res {
        eprintln!("{}", err);
    }
}
