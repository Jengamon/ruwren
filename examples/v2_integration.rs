use ruwren::{wren_module, ModuleLibrary, VMConfig};

mod ext {
    use ruwren::{wren_impl, WrenObject};

    #[derive(WrenObject)]
    struct Unit;

    #[derive(WrenObject)]
    struct NewType(u8);

    #[derive(WrenObject)]
    struct Tuple(u8, u8, #[wren(static_member)] u8, u8);

    #[derive(WrenObject, Debug, Clone)]
    pub struct Foo {
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
        //
        // New WASM considerations:
        // taking in a T means that if Wren passes in *any* other type, the runtime will flip out,
        // prefer taking in Option<T> and handling the None case (the case when Wren tried to give
        // you a data of a type that wasn't the one you declared)
        //
        // FIXED: WASM will no longer flip out or do weird things when something of the
        // wrong type is passed in, it will simply error out on the Wren side.
        //
        // Passing in *nothing* (having a mismatch in arity) will also safely error.
        // Basically, it's all fixed...

        #[wren_impl(constructor)]
        fn construct(&mut self, bar: f64) -> Result<FooInstance, String> {
            Ok(FooInstance { bar })
        }

        // This is only given a FooClass
        #[wren_impl(object(ifoo))]
        pub fn static_fn(&mut self, num: i32, ifoo: Option<Foo>) -> i32 {
            if let Some(foo_inst) = ifoo {
                eprintln!(
                    "got a Foo instance, is self-consistent? {}",
                    foo_inst.sbar == self.sbar
                )
            } else {
                eprintln!("no Foo instance...");
            }
            self.sbar += num;
            self.sbar
        }

        // a static getter
        #[wren_impl(getter)]
        fn sbar(class: &mut FooClass) -> i32 {
            class.sbar
        }

        // an instance getter
        #[wren_impl(instance, setter)]
        fn bar(inst: &mut FooWrapper, nbar: Option<f64>) {
            match nbar {
                Some(val) => {
                    eprintln!("old {} -> new {}", inst.bar, val);
                    inst.bar = val;
                }
                None => eprintln!("arg wasn't a number, ignoring"),
            }
        }

        #[wren_impl(instance)]
        // This is given a full Foo(Wrapper), not FooClass or FooInstance
        fn instance(&self) -> f64 {
            dbg!(self.class.sbar);
            self.bar
        }
    }

    #[derive(WrenObject, Default, Debug)]
    pub struct Teller;

    #[wren_impl]
    impl Teller {
        #[wren_impl(object(foo_inst))]
        fn tell_foo(&self, foo_inst: Option<Foo>) -> String {
            match foo_inst {
                Some(foo_inst) => format!("{:?}", foo_inst),
                None => "that's not a Foo".to_string(),
            }
        }

        #[wren_impl(instance, object(haha))]
        fn teller(&self, haha: Teller) -> String {
            format!("There nothing to tell: {:?}", haha)
        }
    }

    #[derive(WrenObject, Default)]
    pub struct Storage(#[wren(static_member)] Option<Foo>);

    #[wren_impl]
    impl Storage {
        #[wren_impl(object(foo_inst), setter)]
        fn foo(&mut self, foo_inst: Option<Foo>) {
            self.0 = foo_inst;
        }

        #[wren_impl(getter)]
        fn foo(&self) -> Option<Foo> {
            self.0.clone()
        }
    }
}

wren_module! {
    pub mod foobar {
        pub crate::ext::Foo;
        pub crate::ext::Teller;
        pub crate::ext::Storage;
        // We can't make non-pub structs pub, so
        // pub crate::Tuple;
        // won't work
    }
}

const FOOBAR_SRC: &str = include_str!("v2_integration/foobar_full.wren");

fn main() {
    let mut lib = ModuleLibrary::new();
    foobar::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    vm.interpret("foobar", FOOBAR_SRC).unwrap();

    let res = vm.interpret("main", include_str!("v2_integration/main_full.wren"));

    if let Err(err) = res {
        eprintln!("{}", err);
    }
}
