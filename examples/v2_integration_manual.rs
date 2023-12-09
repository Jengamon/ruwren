use ruwren::{
    foreign_v2::{
        get_slot_object, get_slot_value, ForeignItem, InputSlot, V2Class, V2ClassAllocator, WrenTo,
    },
    ClassObject, ModuleLibrary, VMConfig, VM,
};

#[derive(Debug)]
struct Foo {
    bar: f64,
    sbar: i32,
}

impl<'a> From<(&'a FooClass, &'a FooInstance)> for Foo {
    fn from((class, instance): (&'a FooClass, &'a FooInstance)) -> Self {
        Foo {
            bar: instance.bar.clone(),
            sbar: class.sbar.clone(),
        }
    }
}

// Derive macro
struct FooWrapper<'a> {
    _marker: std::marker::PhantomData<&'a ()>,
    class: &'a mut FooClass,
    bar: &'a mut f64,
}

impl<'a> From<(&'a mut FooClass, &'a mut FooInstance)> for FooWrapper<'a> {
    fn from((class, instance): (&'a mut FooClass, &'a mut FooInstance)) -> Self {
        Self {
            _marker: std::marker::PhantomData,
            class,
            bar: &mut instance.bar,
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

struct FooInstance {
    bar: f64,
}

impl From<Foo> for FooInstance {
    fn from(value: Foo) -> Self {
        Self { bar: value.bar }
    }
}

// Impl macro
impl FooClass {
    fn new() -> FooClass {
        FooClass { sbar: 0 }
    }

    fn construct(_class: &mut FooClass, bar: f64) -> FooInstance {
        FooInstance { bar }
    }

    fn sbar(class: &mut FooClass) -> i32 {
        class.sbar
    }

    fn vm_sbar(class: &mut FooClass, vm: &VM) {
        let ret = FooClass::sbar(class);
        WrenTo::to_vm(ret, vm, 0, 1)
    }

    unsafe extern "C" fn native_vm_sbar(vm: *mut ruwren::wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(&format!("Failed to access VM at {:p}", &conf.vm));
        set_hook(Box::new(|_| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            vm_borrow.use_class::<FooInstance, _, _>(|vm, cls| {
                let class =
                    cls.expect(&format!("Failed to resolve class for {}", FooClass::name()));
                FooClass::vm_sbar(class, vm)
            })
        }) {
            Ok(_) => (),
            Err(err) => {
                let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                    strg.clone()
                } else if let Some(strg) = err.downcast_ref::<&str>() {
                    strg.to_string()
                } else {
                    "Non-string panic message".into()
                };

                vm_borrow.set_slot_string(0, err_string);
                vm_borrow.abort_fiber(0);
            }
        };
        drop(take_hook());
        std::ptr::write_unaligned(
            ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
            conf,
        );
    }

    fn static_fn(class: &mut FooClass, num: i32, ifoo: Option<Foo>) -> i32 {
        eprintln!("{:?}", ifoo);
        class.sbar += num;
        class.sbar + 5
    }

    fn vm_static_fn(class: &mut FooClass, vm: &VM) {
        let arg0_calc = InputSlot::new::<_, i32>(1, 2);
        let arg1_calc = InputSlot::object_next(2, &arg0_calc);
        vm.ensure_slots(arg1_calc.scratch_end());

        let arg0 = get_slot_value(vm, &arg0_calc, 2);
        let arg1 = get_slot_object::<FooInstance>(vm, &arg1_calc, 2, class);
        let ret = FooClass::static_fn(class, arg0, arg1);
        WrenTo::to_vm(ret, vm, 0, 1)
    }

    unsafe extern "C" fn native_vm_static_fn(vm: *mut ruwren::wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(&format!("Failed to access VM at {:p}", &conf.vm));
        set_hook(Box::new(|_| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            vm_borrow.use_class::<FooInstance, _, _>(|vm, cls| {
                let class =
                    cls.expect(&format!("Failed to resolve class for {}", FooClass::name()));
                FooClass::vm_static_fn(class, vm)
            })
        }) {
            Ok(_) => (),
            Err(err) => {
                let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                    strg.clone()
                } else if let Some(strg) = err.downcast_ref::<&str>() {
                    strg.to_string()
                } else {
                    "Non-string panic message".into()
                };

                vm_borrow.set_slot_string(0, err_string);
                vm_borrow.abort_fiber(0);
            }
        };
        drop(take_hook());
        std::ptr::write_unaligned(
            ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
            conf,
        );
    }
}

impl<'a> FooWrapper<'a> {
    fn bar(&mut self, val: Option<f64>) {
        match val {
            Some(val) => {
                eprintln!("old {} -> new {}", *self.bar, val);
                *self.bar = val;
            }
            None => eprintln!("arg wasn't a number, ignoring"),
        }
    }

    fn vm_bar(&mut self, vm: &VM) {
        let arg0_calc = InputSlot::new::<_, Option<f64>>(1, 1);
        vm.ensure_slots(arg0_calc.scratch_end());

        let arg0 = get_slot_value(vm, &arg0_calc, 1);
        let ret = self.bar(arg0);
        WrenTo::to_vm(ret, vm, 0, 1)
    }

    unsafe extern "C" fn native_vm_bar(vm: *mut wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(&format!("Failed to access VM at {:p}", &conf.vm));
        set_hook(Box::new(|_pi| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            vm_borrow.ensure_slots(1);
            let inst = vm_borrow
                .get_slot_foreign_mut::<FooInstance>(0)
                .expect(&format!(
                    "Tried to call {0} of {1} on non-{1} type",
                    stringify!($inf),
                    std::any::type_name::<FooInstance>()
                ));
            vm_borrow.use_class::<FooInstance, _, _>(|vm, cls| {
                let class =
                    cls.expect(&format!("Failed to resolve class for {}", FooClass::name()));
                let mut wrapper: FooWrapper = (class, inst).into();
                wrapper.vm_bar(vm)
            })
        }) {
            Ok(_) => (),
            Err(err) => {
                let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                    strg.clone()
                } else if let Some(strg) = err.downcast_ref::<&str>() {
                    strg.to_string()
                } else {
                    "Non-string panic message".into()
                };

                vm_borrow.set_slot_string(0, err_string);
                vm_borrow.abort_fiber(0);
            }
        };
        drop(take_hook());
        std::ptr::write_unaligned(
            ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
            conf,
        );
    }

    fn instance(&self) -> f64 {
        dbg!(&self.class.sbar);
        *self.bar
    }

    fn vm_instance(&self, vm: &VM) {
        let ret = self.instance();
        WrenTo::to_vm(ret, vm, 0, 1)
    }

    unsafe extern "C" fn native_vm_instance(vm: *mut wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(&format!("Failed to access VM at {:p}", &conf.vm));
        set_hook(Box::new(|_pi| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            vm_borrow.ensure_slots(1);
            let inst = vm_borrow
                .get_slot_foreign_mut::<FooInstance>(0)
                .expect(&format!(
                    "Tried to call {0} of {1} on non-{1} type",
                    stringify!($inf),
                    std::any::type_name::<FooInstance>()
                ));
            vm_borrow.use_class::<FooInstance, _, _>(|vm, cls| {
                let class =
                    cls.expect(&format!("Failed to resolve class for {}", FooClass::name()));
                let wrapper: FooWrapper = (class, inst).into();
                wrapper.vm_instance(vm)
            })
        }) {
            Ok(_) => (),
            Err(err) => {
                let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                    strg.clone()
                } else if let Some(strg) = err.downcast_ref::<&str>() {
                    strg.to_string()
                } else {
                    "Non-string panic message".into()
                };

                vm_borrow.set_slot_string(0, err_string);
                vm_borrow.abort_fiber(0);
            }
        };
        drop(take_hook());
        std::ptr::write_unaligned(
            ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
            conf,
        );
    }
}

impl V2Class for FooClass {
    fn name() -> &'static str {
        "Foo"
    }
}

impl V2ClassAllocator for FooClass {
    fn allocate() -> Self {
        FooClass::new()
    }
}

impl ForeignItem for FooInstance {
    type Class = FooClass;
    type Source = Foo;

    fn construct(class: &mut Self::Class, vm: &ruwren::VM) -> Self {
        let arg0_calc = InputSlot::new::<_, f64>(1, 1);
        // let arg1_calc = InputSlot::object_next(1, &arg0_calc);
        vm.ensure_slots(arg0_calc.scratch_end());
        let arg0 = get_slot_value(vm, &arg0_calc, 1);
        // let arg1 = get_slot_object::<Self>(vm, &arg1_calc, class);
        FooClass::construct(class, arg0)
    }
}

impl ClassObject for FooInstance {
    fn initialize_pointer() -> extern "C" fn(*mut wren_sys::WrenVM)
    where
        Self: Sized,
    {
        extern "C" fn _constructor(vm: *mut wren_sys::WrenVM) {
            use ruwren::Class;
            use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
            unsafe {
                let conf = std::ptr::read_unaligned(
                    ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
                );
                let ovm = vm;
                let vm = std::rc::Weak::upgrade(&conf.vm)
                    .expect(&format!("Failed to access VM at {:p}", &conf.vm));
                let wptr = ruwren::wren_sys::wrenSetSlotNewForeign(
                    vm.borrow().vm,
                    0,
                    0,
                    std::mem::size_of::<ruwren::ForeignObject<FooInstance>>()
                        as ruwren::wren_sys::size_t,
                );
                // Allocate a new object, and move it onto the heap
                set_hook(Box::new(|_pi| {}));
                let vm_borrow = AssertUnwindSafe(vm.borrow());
                let object = match catch_unwind(|| <FooInstance as Class>::initialize(&*vm_borrow))
                {
                    Ok(obj) => Some(obj),
                    Err(err) => {
                        let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                            strg.clone()
                        } else if let Some(strg) = err.downcast_ref::<&str>() {
                            strg.to_string()
                        } else {
                            "Non-string panic message".into()
                        };

                        vm_borrow.set_slot_string(0, err_string);
                        vm_borrow.abort_fiber(0);
                        None
                    }
                };
                drop(take_hook());
                // Copy the object pointer if we were successful
                if let Some(object) = object {
                    std::ptr::write(
                        wptr as *mut _,
                        ruwren::ForeignObject {
                            object: Box::into_raw(Box::new(object)),
                            type_id: std::any::TypeId::of::<FooInstance>(),
                        },
                    );
                }
                std::ptr::write_unaligned(
                    ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
                    conf,
                );
            }
        }

        _constructor
    }

    fn finalize_pointer() -> extern "C" fn(*mut std::ffi::c_void)
    where
        Self: Sized,
    {
        extern "C" fn _destructor(data: *mut std::ffi::c_void) {
            unsafe {
                let mut fo: ruwren::ForeignObject<FooInstance> =
                    std::ptr::read_unaligned(data as *mut _);
                if !fo.object.is_null() {
                    _ = Box::from_raw(fo.object);
                }
                fo.object = std::ptr::null_mut();
                std::ptr::write_unaligned(data as *mut _, fo);
            }
        }

        _destructor
    }

    fn generate_pointers() -> ruwren::ClassObjectPointers
    where
        Self: Sized,
    {
        ruwren::ClassObjectPointers {
            function_pointers: vec![
                ruwren::MethodPointer {
                    is_static: true,
                    signature: ruwren::FunctionSignature::new_function("static_fn", 2),
                    pointer: FooClass::native_vm_static_fn,
                },
                ruwren::MethodPointer {
                    is_static: false,
                    signature: ruwren::FunctionSignature::new_function("instance", 0),
                    pointer: FooWrapper::native_vm_instance,
                },
                ruwren::MethodPointer {
                    is_static: true,
                    signature: ruwren::FunctionSignature::new_getter("sbar"),
                    pointer: FooClass::native_vm_sbar,
                },
                ruwren::MethodPointer {
                    is_static: false,
                    signature: ruwren::FunctionSignature::new_setter("bar"),
                    pointer: FooWrapper::native_vm_bar,
                },
            ],
        }
    }
}

mod foobar {
    use std::any::type_name;

    use ruwren::foreign_v2::{Extractable, Slottable, V2Class, WrenAtom, WrenTo};

    fn module_name() -> String {
        stringify!(foobar).replace("_", "/")
    }

    use super::{Foo, FooClass, FooInstance};

    impl WrenTo for Foo {
        fn to_vm(self, vm: &ruwren::VM, slot: ruwren::SlotId, scratch_start: ruwren::SlotId) {
            vm.set_slot_new_foreign_scratch::<_, _, FooInstance>(
                module_name(),
                FooClass::name(),
                self.into(),
                slot,
                scratch_start,
            )
            .unwrap();
        }
    }

    impl Slottable<Foo> for FooInstance {
        type Context = FooClass;
        fn scratch_size() -> usize
        where
            Self: Sized,
        {
            1
        }

        fn get(
            ctx: &mut Self::Context, vm: &ruwren::VM, slot: ruwren::SlotId,
            _scratch_start: ruwren::SlotId,
        ) -> Foo {
            let inst = vm.get_slot_foreign::<Self>(slot).expect(&format!(
                "slot {} is not type {}",
                slot,
                type_name::<Self>()
            ));
            (&*ctx, inst).into()
        }
    }

    pub fn publish_module(lib: &mut ruwren::ModuleLibrary) {
        let mut module = ruwren::Module::new();
        module.class::<FooInstance, _>(FooClass::name());
        lib.module(module_name(), module);
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
