#[derive(WrenObject)]
struct Foo {
    bar: f64,
    #[static_member]
    sbar: i32,
}

#[wren_impl]
impl Foo {
    #[constructor]
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

    #[static_fn]
    fn static_fn(&mut self, num: i32) -> i32 {
        self.sbar += num;
        self.sbar
    }
}

wren_module! {
    mod foobar {
        struct Foo;
    }
}

// Should expand to
#[derive(Default)]
struct FooClass {
    sbar: i32,
}

impl FooClass {
    fn static_fn(&mut self, num: i32) -> i32 {
        self.sbar += num;
        self.sbar
    }

    // A neighboring method is generated for vm interaction
    pub fn vm_static_fn(&mut self, vm: &VM) {
        let method_data = /*generated parameter & return code*/ ();
        let arg0 = method_data.extract::<i32>(vm, 1);
        method_data.returning(vm, Self::static_fn(self, arg0))
    }

    fn new(&mut self, bar: f64) -> Foo {
        Foo { bar }
    }
}

impl Class for FooClass {
    type Instance = Foo;

    fn name(&self) -> &'static str {
        "Foo"
    }

    fn initialize(vm: &VM, class: &mut Self) -> Self::Instance {
        let arg_data = /*generated parameter code*/ ();
        let arg0 = arg_data.extract::<f64>(vm, 1);
        FooClass::new(class, arg0) // you can only have 1 constructor, which determines which function this will be.
    }

    // If no method is marked #[construct], initialize is instead
    fn initialize(_vm: &VM, _class: &mut Self) -> Self::Instance {
        Foo::default()
    }
}

struct Foo {
    bar: f64,
}

impl Registerable for Foo {
    type Class = FooClass;
}

// in runtime
pub fn register_foreign<T: Registerable>(ml: &mut ModuleLibrary) {
    ml.register_foreign_class(T::Class::default())
}

// Class impl
impl Foo {
    fn instance(&self, class: &mut FooClass) -> f64 {
        self.bar
    }

    // A neighboring method is generated for vm interaction
    pub fn vm_static_fn(vm: &VM, class: &mut FooClass) {
        let method_data = /*generated parameter & return code*/ ();
        method_data.returning(vm, Self::instance(self, class))
    }
}

mod foobar {
    fn publish_module(ml: &mut ModuleLibrary) {
        register_foreign::<Foo>(ml)
    }
}