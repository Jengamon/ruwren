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
        Foo { bar, sbar: 0 }
    }

    #[instance]
    // So self *looks* like Foo, but it's actually FooWrapper
    fn instance(&self) -> f64 {
        self.bar
    }

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
struct FooWrapper<'a> {
    bar: &'a mut f64,
    sbar: &'a mut i32,
}

impl<'a> From<(&'a mut FooClass, &'a mut FooInstance)> for FooWrapper<'a> {
    fn from((class, instance): (&'a mut FooClass, &'a mut FooInstance)) -> Self {
        Self {
            bar: &mut instance.bar,
            sbar: &mut class.sbar,
        }
    }
}

struct FooClass {
    sbar: i32,
}

impl From<Foo> for FooClass {
    fn from(value: Foo) -> Self {
        Self { sbar: value.sbar }
    }
}

impl FooClass {
    fn new() -> FooClass {
        Foo { bar, sbar: 0 }
    }

    fn construct(class: &mut FooClass, bar: f64) -> FooInstance {
        FooInstance { bar }
    }

    fn static_fn(&mut self, num: i32) -> i32 {
        *self.sbar += num;
        *self.sbar
    }

    // A neighboring method is generated for vm interaction
    pub fn vm_static_fn(&mut self, vm: &VM) {
        let method_data = /*generated parameter & return code*/ ();
        let arg0 = method_data.extract::<i32>(vm, 1);
        method_data.returning(vm, Self::static_fn(self, arg0))
    }
}

impl V2Class for FooClass {
    type Instance = FooInstance;

    fn name(&self) -> &'static str {
        "Foo"
    }

    // If no method is marked #[construct], initialize is instead
    fn initialize(&mut self, _vm: &VM) -> Self::Instance {
        Foo::default()
    }
}

struct FooInstance {
    bar: f64,
}

impl WrenConvert for FooInstance {
    fn to_vm(self, vm: &VM, slot: usize) {
        vm.set_slot_foreign_v2(slot, self)
    }

    fn from_vm(vm: &VM, slot: usize) -> Self {
        vm.get_slot_foreign_v2(slot)
    }
}

// Class impl
impl FooInstance {
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
        ml.register_foreign_v2::<Foo>(ml)
    }
}
