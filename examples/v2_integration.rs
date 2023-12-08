use ruwren_macros::{wren_impl, wren_module, WrenObject};

#[derive(WrenObject)]
struct Foo {
    bar: f64,
    #[wren_field(static_member)]
    sbar: i32,
}

#[derive(WrenObject)]
struct Unit;

#[derive(WrenObject)]
struct NewType(u8);

#[derive(WrenObject)]
struct Tuple(u8, u8);

#[wren_impl]
impl Foo {
    #[wren_impl(allocator)]
    // The return type of any method marked #[constructor] *must* be Self.
    // Only one method may be marked constructor for any #[wren_impl]
    fn new() -> FooClass {
        Foo { bar, sbar: 0 }
    }

    #[wren_impl(constructor)]
    fn construct(class: &mut FooClass, bar: f64) -> FooInstance {
        FooInstance { bar }
    }

    // This is only given a FooClass
    fn static_fn(class: &mut FooClass, num: i32) -> i32 {
        class.sbar += num;
        class.sbar
    }

    // a static getter
    #[wren_impl(getter)]
    fn sbar(class: &mut FooClass) -> i32 {
        class.sbar
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
        pub Foo;
    }
}

fn main() {}
