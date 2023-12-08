use ruwren_macros::{WrenObject, wren_impl, wren_module};

#[derive(WrenObject)]
struct Foo {
    bar: f64,
    #[wren_field(static_member)]
    sbar: i32,
}

#[wren_impl]
impl Foo {
    #[wren_impl(constructor)]
    // The return type of any method marked #[constructor] *must* be Self.
    // Only one method may be marked constructor for any #[wren_impl]
    fn new(bar: f64) -> Self {
        Foo { bar }
    }

    // You are only allowed to return T, things that can be converted to [T] or [(K, T)] where K and T implement the Wren conversion trait
    // "class: &mut XClass" is injected into the function parameters for a wren object X
    fn instance(&self) -> f64 {
        self.bar
    }

    #[wren_impl(static_fn)]
    fn static_fn(&mut self, num: i32) -> i32 {
        self.sbar += num;
        self.sbar
    }
}

wren_module! {
    mod foobar {
        pub Foo;
    }
}

fn main() {
    
}