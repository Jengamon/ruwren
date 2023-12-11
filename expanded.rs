#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use ruwren::{wren_impl, wren_module, ModuleLibrary, VMConfig, WrenObject};
struct Unit;
impl<'a> From<(&'a UnitClass, &'a UnitInstance)> for Unit {
    fn from((class, inst): (&'a UnitClass, &'a UnitInstance)) -> Self {
        Self
    }
}
impl TryFrom<Option<Unit>> for Unit {
    type Error = ();
    fn try_from(value: Option<Unit>) -> Result<Self, Self::Error> {
        value.ok_or(())
    }
}
struct UnitClass;
impl From<Unit> for UnitClass {
    fn from(source: Unit) -> Self {
        Self
    }
}
struct UnitInstance;
impl From<Unit> for UnitInstance {
    fn from(source: Unit) -> Self {
        Self
    }
}
struct UnitWrapper<'a> {
    class: &'a mut UnitClass,
    instance: &'a mut UnitInstance,
}
impl<'a> From<(&'a mut UnitClass, &'a mut UnitInstance)> for UnitWrapper<'a> {
    fn from((class, instance): (&'a mut UnitClass, &'a mut UnitInstance)) -> Self {
        Self { class, instance }
    }
}
impl<'a> std::ops::Deref for UnitWrapper<'a> {
    type Target = UnitInstance;
    fn deref(&self) -> &UnitInstance {
        &self.instance
    }
}
impl<'a> std::ops::DerefMut for UnitWrapper<'a> {
    fn deref_mut(&mut self) -> &mut UnitInstance {
        &mut self.instance
    }
}
struct NewType(u8);
impl<'a> From<(&'a NewTypeClass, &'a NewTypeInstance)> for NewType {
    fn from((class, inst): (&'a NewTypeClass, &'a NewTypeInstance)) -> Self {
        {
            ::core::panicking::panic_fmt(
                format_args!(
                    "not yet implemented: {0}",
                    format_args!("impl for structs with unnnamed fields"),
                ),
            );
        }
    }
}
impl TryFrom<Option<NewType>> for NewType {
    type Error = ();
    fn try_from(value: Option<NewType>) -> Result<Self, Self::Error> {
        value.ok_or(())
    }
}
struct NewTypeClass;
struct NewTypeInstance;
impl From<NewType> for NewTypeInstance {
    fn from(source: NewType) -> Self {
        {
            ::core::panicking::panic_fmt(
                format_args!(
                    "not yet implemented: {0}",
                    format_args!("From impl for unnamed-fields struct"),
                ),
            );
        }
    }
}
struct NewTypeWrapper<'a> {
    class: &'a mut NewTypeClass,
    instance: &'a mut NewTypeInstance,
}
impl<'a> From<(&'a mut NewTypeClass, &'a mut NewTypeInstance)> for NewTypeWrapper<'a> {
    fn from((class, instance): (&'a mut NewTypeClass, &'a mut NewTypeInstance)) -> Self {
        Self { class, instance }
    }
}
impl<'a> std::ops::Deref for NewTypeWrapper<'a> {
    type Target = NewTypeInstance;
    fn deref(&self) -> &NewTypeInstance {
        &self.instance
    }
}
impl<'a> std::ops::DerefMut for NewTypeWrapper<'a> {
    fn deref_mut(&mut self) -> &mut NewTypeInstance {
        &mut self.instance
    }
}
struct Tuple(u8, u8, #[wren(static_member)] u8, u8);
impl<'a> From<(&'a TupleClass, &'a TupleInstance)> for Tuple {
    fn from((class, inst): (&'a TupleClass, &'a TupleInstance)) -> Self {
        {
            ::core::panicking::panic_fmt(
                format_args!(
                    "not yet implemented: {0}",
                    format_args!("impl for structs with unnnamed fields"),
                ),
            );
        }
    }
}
impl TryFrom<Option<Tuple>> for Tuple {
    type Error = ();
    fn try_from(value: Option<Tuple>) -> Result<Self, Self::Error> {
        value.ok_or(())
    }
}
struct TupleClass;
struct TupleInstance;
impl From<Tuple> for TupleInstance {
    fn from(source: Tuple) -> Self {
        {
            ::core::panicking::panic_fmt(
                format_args!(
                    "not yet implemented: {0}",
                    format_args!("From impl for unnamed-fields struct"),
                ),
            );
        }
    }
}
struct TupleWrapper<'a> {
    class: &'a mut TupleClass,
    instance: &'a mut TupleInstance,
}
impl<'a> From<(&'a mut TupleClass, &'a mut TupleInstance)> for TupleWrapper<'a> {
    fn from((class, instance): (&'a mut TupleClass, &'a mut TupleInstance)) -> Self {
        Self { class, instance }
    }
}
impl<'a> std::ops::Deref for TupleWrapper<'a> {
    type Target = TupleInstance;
    fn deref(&self) -> &TupleInstance {
        &self.instance
    }
}
impl<'a> std::ops::DerefMut for TupleWrapper<'a> {
    fn deref_mut(&mut self) -> &mut TupleInstance {
        &mut self.instance
    }
}
struct Foo {
    bar: f64,
    #[wren(static_member)]
    sbar: i32,
}
impl<'a> From<(&'a FooClass, &'a FooInstance)> for Foo {
    fn from((class, inst): (&'a FooClass, &'a FooInstance)) -> Self {
        Self {
            bar: inst.bar.clone(),
            sbar: class.sbar.clone(),
        }
    }
}
impl TryFrom<Option<Foo>> for Foo {
    type Error = ();
    fn try_from(value: Option<Foo>) -> Result<Self, Self::Error> {
        value.ok_or(())
    }
}
struct FooClass {
    sbar: i32,
}
impl From<Foo> for FooClass {
    fn from(source: Foo) -> Self {
        Self { sbar: source.sbar }
    }
}
struct FooInstance {
    bar: f64,
}
impl From<Foo> for FooInstance {
    fn from(source: Foo) -> Self {
        Self { bar: source.bar }
    }
}
struct FooWrapper<'a> {
    class: &'a mut FooClass,
    instance: &'a mut FooInstance,
}
impl<'a> From<(&'a mut FooClass, &'a mut FooInstance)> for FooWrapper<'a> {
    fn from((class, instance): (&'a mut FooClass, &'a mut FooInstance)) -> Self {
        Self { class, instance }
    }
}
impl<'a> std::ops::Deref for FooWrapper<'a> {
    type Target = FooInstance;
    fn deref(&self) -> &FooInstance {
        &self.instance
    }
}
impl<'a> std::ops::DerefMut for FooWrapper<'a> {
    fn deref_mut(&mut self) -> &mut FooInstance {
        &mut self.instance
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Foo {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field2_finish(
            f,
            "Foo",
            "bar",
            &self.bar,
            "sbar",
            &&self.sbar,
        )
    }
}
impl FooClass {
    fn new() -> FooClass {
        FooClass { sbar: 0 }
    }
    fn vm_construct(&mut self, vm: &ruwren::VM) -> FooInstance {
        let arg0_calc = ruwren::foreign_v2::InputSlot::new::<_, f64>(1usize, 1usize);
        vm.ensure_slots(arg0_calc.scratch_end());
        let arg0: f64 = ruwren::foreign_v2::get_slot_value(vm, &arg0_calc, 1usize);
        let ret = FooClass::construct(self, arg0);
        ret
    }
    fn construct(&mut self, bar: f64) -> FooInstance {
        FooInstance { bar }
    }
    fn vm_static_fn(&mut self, vm: &ruwren::VM) {
        let arg0_calc = ruwren::foreign_v2::InputSlot::new::<_, i32>(1usize, 2usize);
        let arg1_calc = ruwren::foreign_v2::InputSlot::object_next(2usize, &arg0_calc);
        vm.ensure_slots(arg1_calc.scratch_end());
        let arg0: i32 = ruwren::foreign_v2::get_slot_value(vm, &arg0_calc, 2usize);
        let arg1 = ruwren::foreign_v2::get_slot_object::<
            FooInstance,
        >(vm, &arg1_calc, 2usize, self);
        let ret = FooClass::static_fn(
            self,
            arg0,
            match arg1.try_into() {
                Ok(v) => v,
                Err(_) => {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "slot {0} cannot be type {1}",
                            2,
                            std::any::type_name::<Option<Foo>>(),
                        ),
                    );
                }
            },
        );
        ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
    }
    unsafe extern "C" fn native_vm_static_fn(vm: *mut ruwren::wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(
                &{
                    let res = ::alloc::fmt::format(
                        format_args!("Failed to access VM at {0:p}", &conf.vm),
                    );
                    res
                },
            );
        set_hook(Box::new(|_| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            use ruwren::foreign_v2::V2Class;
            vm_borrow
                .use_class_mut::<
                    FooInstance,
                    _,
                    _,
                >(|vm, cls| {
                    let class = cls
                        .expect(
                            &{
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Failed to resolve class for {0}",
                                        FooClass::name(),
                                    ),
                                );
                                res
                            },
                        );
                    FooClass::vm_static_fn(class, vm)
                })
        }) {
            Ok(_) => {}
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
    fn static_fn(&mut self, num: i32, ifoo: Option<Foo>) -> i32 {
        if let Some(foo) = ifoo {
            {
                ::std::io::_eprint(
                    format_args!(
                        "got a Foo instance, is self-consistent? {0}\n",
                        foo.sbar == self.sbar,
                    ),
                );
            }
        } else {
            {
                ::std::io::_eprint(format_args!("no Foo instance...\n"));
            };
        }
        self.sbar += num;
        self.sbar
    }
    fn vm_sbar(&mut self, vm: &ruwren::VM) {
        let ret = FooClass::sbar(self);
        ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
    }
    unsafe extern "C" fn native_vm_sbar(vm: *mut ruwren::wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(
                &{
                    let res = ::alloc::fmt::format(
                        format_args!("Failed to access VM at {0:p}", &conf.vm),
                    );
                    res
                },
            );
        set_hook(Box::new(|_| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            use ruwren::foreign_v2::V2Class;
            vm_borrow
                .use_class_mut::<
                    FooInstance,
                    _,
                    _,
                >(|vm, cls| {
                    let class = cls
                        .expect(
                            &{
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Failed to resolve class for {0}",
                                        FooClass::name(),
                                    ),
                                );
                                res
                            },
                        );
                    FooClass::vm_sbar(class, vm)
                })
        }) {
            Ok(_) => {}
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
    fn sbar(&mut self) -> i32 {
        self.sbar
    }
}
impl<'a> FooWrapper<'a> {
    fn vm_bar(&mut self, vm: &ruwren::VM) {
        let arg0_calc = ruwren::foreign_v2::InputSlot::new::<
            _,
            Option<f64>,
        >(1usize, 1usize);
        vm.ensure_slots(arg0_calc.scratch_end());
        let arg0: Option<f64> = ruwren::foreign_v2::get_slot_value(
            vm,
            &arg0_calc,
            1usize,
        );
        let ret = FooWrapper::bar(self, arg0);
        ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
    }
    unsafe extern "C" fn native_vm_bar(vm: *mut wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(
                &{
                    let res = ::alloc::fmt::format(
                        format_args!("Failed to access VM at {0:p}", &conf.vm),
                    );
                    res
                },
            );
        set_hook(Box::new(|_pi| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            use ruwren::foreign_v2::V2Class;
            vm_borrow.ensure_slots(1);
            let inst = vm_borrow
                .get_slot_foreign_mut::<FooInstance>(0)
                .expect(
                    &{
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "Tried to call {0} of {1} on non-{1} type",
                                "$inf",
                                std::any::type_name::<FooInstance>(),
                            ),
                        );
                        res
                    },
                );
            vm_borrow
                .use_class_mut::<
                    FooInstance,
                    _,
                    _,
                >(|vm, cls| {
                    let class = cls
                        .expect(
                            &{
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Failed to resolve class for {0}",
                                        FooClass::name(),
                                    ),
                                );
                                res
                            },
                        );
                    let mut wrapper: FooWrapper = (class, inst).into();
                    wrapper.vm_bar(vm)
                })
        }) {
            Ok(_) => {}
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
    fn bar(&mut self, nbar: Option<f64>) {
        match nbar {
            Some(val) => {
                {
                    ::std::io::_eprint(
                        format_args!("old {0} -> new {1}\n", self.bar, val),
                    );
                };
                self.bar = val;
            }
            None => {
                ::std::io::_eprint(format_args!("arg wasn\'t a number, ignoring\n"));
            }
        }
    }
    fn vm_instance(&self, vm: &ruwren::VM) {
        let ret = FooWrapper::instance(self);
        ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
    }
    unsafe extern "C" fn native_vm_instance(vm: *mut wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(
                &{
                    let res = ::alloc::fmt::format(
                        format_args!("Failed to access VM at {0:p}", &conf.vm),
                    );
                    res
                },
            );
        set_hook(Box::new(|_pi| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            use ruwren::foreign_v2::V2Class;
            vm_borrow.ensure_slots(1);
            let inst = vm_borrow
                .get_slot_foreign_mut::<FooInstance>(0)
                .expect(
                    &{
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "Tried to call {0} of {1} on non-{1} type",
                                "$inf",
                                std::any::type_name::<FooInstance>(),
                            ),
                        );
                        res
                    },
                );
            vm_borrow
                .use_class_mut::<
                    FooInstance,
                    _,
                    _,
                >(|vm, cls| {
                    let class = cls
                        .expect(
                            &{
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Failed to resolve class for {0}",
                                        FooClass::name(),
                                    ),
                                );
                                res
                            },
                        );
                    let wrapper: FooWrapper = (class, inst).into();
                    wrapper.vm_instance(vm)
                })
        }) {
            Ok(_) => {}
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
        match self.class.sbar {
            tmp => {
                {
                    ::std::io::_eprint(
                        format_args!(
                            "[{0}:{1}] {2} = {3:#?}\n",
                            "examples/v2_integration.rs",
                            76u32,
                            "self.class.sbar",
                            &tmp,
                        ),
                    );
                };
                tmp
            }
        };
        self.bar
    }
}
impl ruwren::foreign_v2::Slottable<Foo> for FooInstance {
    type Context = FooClass;
    fn scratch_size() -> usize
    where
        Self: Sized,
    {
        0
    }
    fn get(
        ctx: &mut Self::Context,
        vm: &ruwren::VM,
        slot: ruwren::SlotId,
        _scratch_start: ruwren::SlotId,
    ) -> Option<Foo> {
        let inst = vm.get_slot_foreign::<Self>(slot)?;
        Some((&*ctx, inst).into())
    }
}
impl ruwren::ClassObject for FooInstance {
    fn initialize_pointer() -> extern "C" fn(*mut wren_sys::WrenVM)
    where
        Self: Sized,
    {
        extern "C" fn _constructor(vm: *mut wren_sys::WrenVM) {
            use ruwren::Class;
            use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
            unsafe {
                let conf = std::ptr::read_unaligned(
                    ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
                );
                let ovm = vm;
                let vm = std::rc::Weak::upgrade(&conf.vm)
                    .expect(
                        &{
                            let res = ::alloc::fmt::format(
                                format_args!("Failed to access VM at {0:p}", &conf.vm),
                            );
                            res
                        },
                    );
                let wptr = ruwren::wren_sys::wrenSetSlotNewForeign(
                    vm.borrow().vm,
                    0,
                    0,
                    std::mem::size_of::<ruwren::ForeignObject<FooInstance>>()
                        as ruwren::wren_sys::size_t,
                );
                set_hook(Box::new(|_pi| {}));
                let vm_borrow = AssertUnwindSafe(vm.borrow());
                let object = match catch_unwind(|| <FooInstance as Class>::initialize(
                    &*vm_borrow,
                )) {
                    Ok(obj) => Some(obj),
                    Err(err) => {
                        let err_string = if let Some(strg) = err.downcast_ref::<String>()
                        {
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
                let mut fo: ruwren::ForeignObject<FooInstance> = std::ptr::read_unaligned(
                    data as *mut _,
                );
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
            function_pointers: <[_]>::into_vec(
                #[rustc_box]
                ::alloc::boxed::Box::new([
                    ruwren::MethodPointer {
                        is_static: true,
                        signature: ruwren::FunctionSignature::new_function(
                            "static_fn",
                            2usize,
                        ),
                        pointer: FooClass::native_vm_static_fn,
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
                    ruwren::MethodPointer {
                        is_static: false,
                        signature: ruwren::FunctionSignature::new_function(
                            "instance",
                            0usize,
                        ),
                        pointer: FooWrapper::native_vm_instance,
                    },
                ]),
            ),
        }
    }
}
impl ruwren::foreign_v2::V2Class for FooClass {
    fn name() -> &'static str {
        "Foo"
    }
}
impl ruwren::foreign_v2::V2ClassAllocator for FooClass {
    fn allocate() -> Self {
        FooClass::new()
    }
}
impl ruwren::foreign_v2::ForeignItem for FooInstance {
    type Class = FooClass;
    type Source = Foo;
    fn construct(class: &mut Self::Class, vm: &ruwren::VM) -> Self {
        FooClass::vm_construct(class, vm)
    }
}
struct Teller;
impl<'a> From<(&'a TellerClass, &'a TellerInstance)> for Teller {
    fn from((class, inst): (&'a TellerClass, &'a TellerInstance)) -> Self {
        Self
    }
}
impl TryFrom<Option<Teller>> for Teller {
    type Error = ();
    fn try_from(value: Option<Teller>) -> Result<Self, Self::Error> {
        value.ok_or(())
    }
}
struct TellerClass;
impl From<Teller> for TellerClass {
    fn from(source: Teller) -> Self {
        Self
    }
}
struct TellerInstance;
impl From<Teller> for TellerInstance {
    fn from(source: Teller) -> Self {
        Self
    }
}
struct TellerWrapper<'a> {
    class: &'a mut TellerClass,
    instance: &'a mut TellerInstance,
}
impl<'a> From<(&'a mut TellerClass, &'a mut TellerInstance)> for TellerWrapper<'a> {
    fn from((class, instance): (&'a mut TellerClass, &'a mut TellerInstance)) -> Self {
        Self { class, instance }
    }
}
impl<'a> std::ops::Deref for TellerWrapper<'a> {
    type Target = TellerInstance;
    fn deref(&self) -> &TellerInstance {
        &self.instance
    }
}
impl<'a> std::ops::DerefMut for TellerWrapper<'a> {
    fn deref_mut(&mut self) -> &mut TellerInstance {
        &mut self.instance
    }
}
#[automatically_derived]
impl ::core::default::Default for Teller {
    #[inline]
    fn default() -> Teller {
        Teller {}
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Teller {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::write_str(f, "Teller")
    }
}
impl TellerClass {
    fn ___default_alloc() -> TellerClass {
        use std::default::Default;
        Teller::default().into()
    }
    fn ___default_constructor(&self) -> TellerInstance {
        use std::default::Default;
        Teller::default().into()
    }
    fn vm_tell_foo(&mut self, vm: &ruwren::VM) {
        let arg0_calc = ruwren::foreign_v2::InputSlot::object_new(1usize, 1usize);
        vm.ensure_slots(arg0_calc.scratch_end());
        let arg0 = ruwren::foreign_v2::get_slot_object::<
            FooInstance,
        >(vm, &arg0_calc, 1usize, self);
        let ret = TellerClass::tell_foo(
            self,
            match arg0.try_into() {
                Ok(v) => v,
                Err(_) => {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "slot {0} cannot be type {1}",
                            2,
                            std::any::type_name::<Option<Foo>>(),
                        ),
                    );
                }
            },
        );
        ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
    }
    unsafe extern "C" fn native_vm_tell_foo(vm: *mut ruwren::wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(
                &{
                    let res = ::alloc::fmt::format(
                        format_args!("Failed to access VM at {0:p}", &conf.vm),
                    );
                    res
                },
            );
        set_hook(Box::new(|_| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            use ruwren::foreign_v2::V2Class;
            vm_borrow
                .use_class_mut::<
                    TellerInstance,
                    _,
                    _,
                >(|vm, cls| {
                    let class = cls
                        .expect(
                            &{
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Failed to resolve class for {0}",
                                        TellerClass::name(),
                                    ),
                                );
                                res
                            },
                        );
                    TellerClass::vm_tell_foo(class, vm)
                })
        }) {
            Ok(_) => {}
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
    fn tell_foo(&self, foo: Option<Foo>) -> String {
        match foo {
            Some(foo) => {
                let res = ::alloc::fmt::format(format_args!("{0:?}", foo));
                res
            }
            None => "that's not a Foo".to_string(),
        }
    }
}
impl<'a> TellerWrapper<'a> {
    fn vm_teller(&self, vm: &ruwren::VM) {
        let arg0_calc = ruwren::foreign_v2::InputSlot::object_new(1usize, 1usize);
        vm.ensure_slots(arg0_calc.scratch_end());
        let arg0 = ruwren::foreign_v2::get_slot_object::<
            TellerInstance,
        >(vm, &arg0_calc, 1usize, self.class);
        let ret = TellerWrapper::teller(
            self,
            match arg0.try_into() {
                Ok(v) => v,
                Err(_) => {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "slot {0} cannot be type {1}",
                            2,
                            std::any::type_name::<Teller>(),
                        ),
                    );
                }
            },
        );
        ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
    }
    unsafe extern "C" fn native_vm_teller(vm: *mut wren_sys::WrenVM) {
        use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
        let conf = std::ptr::read_unaligned(
            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
        );
        let ovm = vm;
        let vm = std::rc::Weak::upgrade(&conf.vm)
            .expect(
                &{
                    let res = ::alloc::fmt::format(
                        format_args!("Failed to access VM at {0:p}", &conf.vm),
                    );
                    res
                },
            );
        set_hook(Box::new(|_pi| {}));
        let vm_borrow = AssertUnwindSafe(vm.borrow());
        match catch_unwind(|| {
            use ruwren::foreign_v2::V2Class;
            vm_borrow.ensure_slots(1);
            let inst = vm_borrow
                .get_slot_foreign_mut::<TellerInstance>(0)
                .expect(
                    &{
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "Tried to call {0} of {1} on non-{1} type",
                                "$inf",
                                std::any::type_name::<TellerInstance>(),
                            ),
                        );
                        res
                    },
                );
            vm_borrow
                .use_class_mut::<
                    TellerInstance,
                    _,
                    _,
                >(|vm, cls| {
                    let class = cls
                        .expect(
                            &{
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Failed to resolve class for {0}",
                                        TellerClass::name(),
                                    ),
                                );
                                res
                            },
                        );
                    let wrapper: TellerWrapper = (class, inst).into();
                    wrapper.vm_teller(vm)
                })
        }) {
            Ok(_) => {}
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
    fn teller(&self, haha: Teller) -> String {
        {
            let res = ::alloc::fmt::format(
                format_args!("There nothing to tell: {0:?}", haha),
            );
            res
        }
    }
}
impl ruwren::foreign_v2::Slottable<Teller> for TellerInstance {
    type Context = TellerClass;
    fn scratch_size() -> usize
    where
        Self: Sized,
    {
        0
    }
    fn get(
        ctx: &mut Self::Context,
        vm: &ruwren::VM,
        slot: ruwren::SlotId,
        _scratch_start: ruwren::SlotId,
    ) -> Option<Teller> {
        let inst = vm.get_slot_foreign::<Self>(slot)?;
        Some((&*ctx, inst).into())
    }
}
impl ruwren::ClassObject for TellerInstance {
    fn initialize_pointer() -> extern "C" fn(*mut wren_sys::WrenVM)
    where
        Self: Sized,
    {
        extern "C" fn _constructor(vm: *mut wren_sys::WrenVM) {
            use ruwren::Class;
            use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
            unsafe {
                let conf = std::ptr::read_unaligned(
                    ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData,
                );
                let ovm = vm;
                let vm = std::rc::Weak::upgrade(&conf.vm)
                    .expect(
                        &{
                            let res = ::alloc::fmt::format(
                                format_args!("Failed to access VM at {0:p}", &conf.vm),
                            );
                            res
                        },
                    );
                let wptr = ruwren::wren_sys::wrenSetSlotNewForeign(
                    vm.borrow().vm,
                    0,
                    0,
                    std::mem::size_of::<ruwren::ForeignObject<TellerInstance>>()
                        as ruwren::wren_sys::size_t,
                );
                set_hook(Box::new(|_pi| {}));
                let vm_borrow = AssertUnwindSafe(vm.borrow());
                let object = match catch_unwind(|| <TellerInstance as Class>::initialize(
                    &*vm_borrow,
                )) {
                    Ok(obj) => Some(obj),
                    Err(err) => {
                        let err_string = if let Some(strg) = err.downcast_ref::<String>()
                        {
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
                if let Some(object) = object {
                    std::ptr::write(
                        wptr as *mut _,
                        ruwren::ForeignObject {
                            object: Box::into_raw(Box::new(object)),
                            type_id: std::any::TypeId::of::<TellerInstance>(),
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
                let mut fo: ruwren::ForeignObject<TellerInstance> = std::ptr::read_unaligned(
                    data as *mut _,
                );
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
            function_pointers: <[_]>::into_vec(
                #[rustc_box]
                ::alloc::boxed::Box::new([
                    ruwren::MethodPointer {
                        is_static: true,
                        signature: ruwren::FunctionSignature::new_function(
                            "tell_foo",
                            1usize,
                        ),
                        pointer: TellerClass::native_vm_tell_foo,
                    },
                    ruwren::MethodPointer {
                        is_static: false,
                        signature: ruwren::FunctionSignature::new_function(
                            "teller",
                            1usize,
                        ),
                        pointer: TellerWrapper::native_vm_teller,
                    },
                ]),
            ),
        }
    }
}
impl ruwren::foreign_v2::V2Class for TellerClass {
    fn name() -> &'static str {
        "Teller"
    }
}
impl ruwren::foreign_v2::V2ClassAllocator for TellerClass {
    fn allocate() -> Self {
        TellerClass::___default_alloc()
    }
}
impl ruwren::foreign_v2::ForeignItem for TellerInstance {
    type Class = TellerClass;
    type Source = Teller;
    fn construct(class: &mut Self::Class, vm: &ruwren::VM) -> Self {
        TellerClass::___default_constructor(class)
    }
}
mod foobar {
    use ruwren::foreign_v2::V2Class;
    fn module_name() -> String {
        "foobar".replace("_", "/")
    }
    impl ruwren::foreign_v2::WrenTo for crate::Foo {
        fn to_vm(
            self,
            vm: &ruwren::VM,
            slot: ruwren::SlotId,
            scratch_start: ruwren::SlotId,
        ) {
            vm.set_slot_new_foreign_scratch::<
                    _,
                    _,
                    crate::FooInstance,
                >(
                    module_name(),
                    crate::FooClass::name(),
                    self.into(),
                    slot,
                    scratch_start,
                )
                .unwrap();
        }
    }
    impl ruwren::foreign_v2::WrenTo for crate::Teller {
        fn to_vm(
            self,
            vm: &ruwren::VM,
            slot: ruwren::SlotId,
            scratch_start: ruwren::SlotId,
        ) {
            vm.set_slot_new_foreign_scratch::<
                    _,
                    _,
                    crate::TellerInstance,
                >(
                    module_name(),
                    crate::TellerClass::name(),
                    self.into(),
                    slot,
                    scratch_start,
                )
                .unwrap();
        }
    }
    pub fn publish_module(lib: &mut ruwren::ModuleLibrary) {
        let mut module = ruwren::Module::new();
        {
            module.class::<crate::FooInstance, _>(crate::FooClass::name());
            module.class::<crate::TellerInstance, _>(crate::TellerClass::name());
        }
        lib.module(module_name(), module);
    }
}
const FOOBAR_SRC: &'static str = "foreign class Foo {\n    construct new(bar) {}\n    foreign bar=(val)\n    foreign static sbar\n    foreign instance()\n    foreign static static_fn(num, foo)\n\n    toString { super.toString + \" { bar = %(this.instance()), sbar = %(type.sbar) }\" }\n}\n\nforeign class Teller {\n    construct new() {}\n    foreign static tell_foo(t)\n    foreign teller(teller)\n}";
fn main() {
    let mut lib = ModuleLibrary::new();
    foobar::publish_module(&mut lib);
    let vm = VMConfig::new().library(&lib).build();
    vm.interpret("foobar", FOOBAR_SRC).unwrap();
    let res = vm
        .interpret(
            "main",
            "import \"foobar\" for Foo, Teller\n\nvar f = Foo.new(4)\nSystem.print(Foo.static_fn(f.instance(), f))\nSystem.print(Foo.sbar)\nSystem.print(f)\nFoo.static_fn(38, null)\nSystem.print(Foo.sbar)\nf.bar = \"42\"\nf.bar = 42\n\nSystem.print(Teller.tell_foo(null))\nSystem.print(Teller.tell_foo(f))\nvar t = Teller.new()\nSystem.print(t.teller(t))",
        );
    if let Err(err) = res {
        {
            ::std::io::_eprint(format_args!("{0}\n", err));
        };
    }
}
