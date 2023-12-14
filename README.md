# Ruwren: Wren bindings for Rust [![Crates.io](https://img.shields.io/crates/v/ruwren)](https://crates.io/crates/ruwren) [![docs.rs](https://docs.rs/ruwren/badge.svg)](https://docs.rs/ruwren/)

Build status: [![Travis CI](https://travis-ci.com/Jengamon/ruwren.svg?branch=master)](https://travis-ci.com/github/Jengamon/ruwren)

Here is an attempt at making some Rust Wren bindings in a more Rust style.
It acts at times pretty transparently (you do have to deal with the slot / foreign API), but should be

- More typesafe than using the C API directly
- Shouldn't get in the way of quick execution
- Should be relatively simple to get started

## Including

Just add:

```toml
ruwren = "0.4"
```

to your Cargo.toml.

## Main API

### Creating a VM

To create a VM, use the VMConfig struct. To create a basic VM that logs it's output to the console, simply call

```rust
let vm = VMConfig::new().build();
```

You can run code by using interpret directly:

```rust
vm.interpret("main", r##"System.print("Cool beans!")"##).unwrap();
```

Which returns a `Ok(())` on successful execution, and `Err(e)` on failure (see VMError for more details).

You can also call code defined in Wren using a FunctionHandle like so:

```rust
vm.interpret("main", r##"
class GameEngine {
    static update(delta) {
        System.print(delta)
    }
}
"##).unwrap();
let handle = vm.make_call_handle(FunctionSignature::new_function("update", 1));
vm.execute(|vm| {
    vm.ensure_slots(2);
    vm.get_variable("main", "GameEngine", 0);
    vm.set_slot_double(1, 0.016);
});
vm.call_handle(&handle);
```

or more directly:

```rust
vm.interpret("main", r##"
class GameEngine {
    static update(delta) {
        System.print(delta)
    }
}
"##).unwrap();
vm.execute(|vm| {
    vm.ensure_slots(2);
    vm.get_variable("main", "GameEngine", 0);
    vm.set_slot_double(1, 0.016);
});
vm.call(FunctionSignature::new_function("update", 1));
```

## Embedding Rust code in Wren

Here's a short example of how you can embed your Rust data into Wren:

```rust
use ruwren::{Class, VM, VMConfig, ModuleLibrary, get_slot_checked, create_module};
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
    vm.interpret("foobar", r##"
    foreign class Foo {
        construct new(bar) {}
        foreign instance()
        foreign static static_fn(num)
    }
    "##).unwrap();

    // You could now write Wren code like:

    vm.interpret("main", r##"
    import "foobar" for Foo
    var f = Foo.new(4)
    System.print(Foo.static_fn(f.instance()))
    "##).unwrap();

    // This should print "9".
}
```

### V2 Foreign

V2 foreigns emulate Wren's class system on top of the original
foreign API, so the above example would be:
```rust
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
    fn constructor(&self, bar: f64) -> FooInstance {
        FooInstance { bar }
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
    vm.interpret("foobar", r##"
    foreign class Foo {
        construct new(bar) {}
        foreign instance()
        foreign static static_fn(num)
    }
    "##).unwrap();

    // You could now write Wren code like:

    vm.interpret("main", r##"
    import "foobar" for Foo
    var f = Foo.new(4)
    System.print(Foo.static_fn(f.instance()))
    "##).unwrap();

    // This should print "9".
}
```

## About WASM Compilation

It technically works as long as you target WASI, and have a WASI SDK setup somewhere.
look at the justfile or example-wasi.nu for the environment variables to set to get it running. There is one big caveat tho:

WASM (even with WASI) is a panic=abort platform, so catch_unwind does nothing,
and panics are unhandleable.

This means that some idioms of the v1 foreign API (namely `get_slot_checked!`) are not very good on the platform. Basically, anything that panics doesn't work well.

With a minimal change the the v2 foreign API (namely, having the constructor be fallible) means that v2 should work relatively unchanged on web, and v1 is *usable*, it just shouldn't trigger a panic, or the wasm runtime will flip the table.
