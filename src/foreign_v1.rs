/// Creates a Wren module
///
/// Creates a function at $modl::publish_module, that takes a `&mut `[`ModuleLibrary`]
/// and handles [`Module`] object creation and registration
///
/// Also internally creates all the necessary extern "C" functions for Wren's callbacks
///
/// See examples folder for the syntax
#[macro_export]
macro_rules! create_module {
    (
        $(
            class($mname:expr) $name:ty => $md:ident {
                $(
                    $si:ident($lbls:ident $($sgns:expr),+) $id:ident
                ),*
            }
        )+

        module => $modl:ident
    ) => {
        $(
            mod $md {
                use std::panic::{take_hook, set_hook, catch_unwind, AssertUnwindSafe};

                pub(in super) extern "C" fn _constructor(vm: *mut $crate::wren_sys::WrenVM) {
                    use $crate::Class;
                    unsafe {
                        let conf = std::ptr::read_unaligned($crate::wren_sys::wrenGetUserData(vm) as *mut $crate::UserData);
                        let ovm = vm;
                        let vm = std::rc::Weak::upgrade(&conf.vm).expect(&format!("Failed to access VM at {:p}", &conf.vm));
                        let wptr = $crate::wren_sys::wrenSetSlotNewForeign(vm.borrow().vm, 0, 0, std::mem::size_of::<$crate::ForeignObject<$name>>());
                        // Allocate a new object, and move it onto the heap
                        set_hook(Box::new(|_pi| {}));
                        let vm_borrow = AssertUnwindSafe(vm.borrow());
                        let object = match catch_unwind(|| <$name as Class>::initialize(&*vm_borrow)) {
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
                            std::ptr::write(wptr as *mut _, $crate::ForeignObject {
                                object: Box::into_raw(Box::new(object)),
                                type_id: std::any::TypeId::of::<$name>(),
                            });
                        }
                        std::ptr::write_unaligned($crate::wren_sys::wrenGetUserData(ovm) as *mut $crate::UserData, conf);
                    }
                }

                pub(in super) extern "C" fn _destructor(data: *mut std::ffi::c_void) {
                    unsafe {
                        let mut fo: $crate::ForeignObject<$name> = std::ptr::read_unaligned(data as *mut _);
                        if !fo.object.is_null() {
                            _ = Box::from_raw(fo.object);
                        }
                        fo.object = std::ptr::null_mut();
                        std::ptr::write_unaligned(data as *mut _, fo);
                    }
                }

                $(
                    $crate::create_module!(@fn $si $name => $id);
                )*
            }

            impl $crate::ClassObject for $name {
                fn initialize_pointer() -> extern "C" fn(*mut $crate::wren_sys::WrenVM) { $md::_constructor }
                fn finalize_pointer() -> extern "C" fn(*mut std::ffi::c_void) { $md::_destructor }
                fn generate_pointers() -> $crate::ClassObjectPointers {
                    $crate::ClassObjectPointers {
                        function_pointers: vec![
                            $(
                                $crate::create_module!(@md $si $id $lbls $md $($sgns),+)
                            ),*
                        ]
                    }
                }
            }
        )+

        pub mod $modl {
            pub fn publish_module(lib: &mut $crate::ModuleLibrary) {
                let mut module = $crate::Module::new();
                module
                $(
                    .class::<$name, _>($mname)
                )+;
                lib.module(stringify!($modl).replace("_", "/"), module);
            }
        }
    };

    (@md static $id:ident $lbls:ident $md:ident $($sgns: expr),+) => {
        $crate::MethodPointer {
            pointer: $md::$id,
            signature: $crate::create_module!(@sgn $lbls $($sgns),+),
            is_static: true,
        }
    };

    (@md instance $id:ident $lbls:ident $md:ident $($sgns: expr),+) => {
        $crate::MethodPointer {
            pointer: $md::$id,
            signature: $crate::create_module!(@sgn $lbls $($sgns),+),
            is_static: false,
        }
    };

    (@sgn fn $nom:expr, $arity:expr) => {
        $crate::FunctionSignature::new_function($nom, $arity)
    };

    (@sgn getter $name:expr) => {
        $crate::FunctionSignature::new_getter($name)
    };

    (@sgn setter $name:expr) => {
        $crate::FunctionSignature::new_setter($name)
    };

    (@fn static $name:ty => $s:ident) => {
        pub(in super) unsafe extern "C" fn $s(vm: *mut $crate::wren_sys::WrenVM) {
            use std::panic::{take_hook, set_hook, catch_unwind, AssertUnwindSafe};

            let conf = std::ptr::read_unaligned($crate::wren_sys::wrenGetUserData(vm) as *mut $crate::UserData);
            let ovm = vm;
            let vm = std::rc::Weak::upgrade(&conf.vm).expect(&format!("Failed to access VM at {:p}", &conf.vm));
            set_hook(Box::new(|_| {}));
            let vm_borrow = AssertUnwindSafe(vm.borrow());
            match catch_unwind(|| <$name>::$s(&*vm_borrow)) {
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
            std::ptr::write_unaligned($crate::wren_sys::wrenGetUserData(ovm) as *mut $crate::UserData, conf);
        }
    };

    (@fn instance $name:ty => $inf:ident) => {
        pub(in super) unsafe extern "C" fn $inf(vm: *mut $crate::wren_sys::WrenVM) {
            use std::panic::{take_hook, set_hook, catch_unwind, AssertUnwindSafe};

            let conf = std::ptr::read_unaligned($crate::wren_sys::wrenGetUserData(vm) as *mut $crate::UserData);
            let ovm = vm;
            let vm = std::rc::Weak::upgrade(&conf.vm).expect(&format!("Failed to access VM at {:p}", &conf.vm));
            set_hook(Box::new(|_pi| {}));
            let vm_borrow = AssertUnwindSafe(vm.borrow());
            match catch_unwind(|| {
                vm_borrow.ensure_slots(1);
                let inst = vm_borrow.get_slot_foreign_mut::<$name>(0)
                    .expect(&format!("Tried to call {0} of {1} on non-{1} type", stringify!($inf), std::any::type_name::<$name>()));
                inst.$inf(&*vm_borrow)
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
            std::ptr::write_unaligned($crate::wren_sys::wrenGetUserData(ovm) as *mut $crate::UserData, conf);
        }
    }
}

/// Checks if the slot type is correct at the given slot.
/// If not, will panic.
/// If it is, will return the item at the given slot.
// We can do unwraps because we manually check the type beforehand, so we are *sure* it is there.
#[macro_export]
macro_rules! get_slot_checked {
    ($vm:expr => num $slot:expr) => {{
        if $vm.get_slot_type($slot) != $crate::SlotType::Num {
            panic!(
                "rust error [{}:{}]: Slot {} is not a <num>",
                file!(),
                line!(),
                $slot
            )
        }
        $vm.get_slot_double($slot).unwrap()
    }};

    ($vm:expr => bool $slot:expr) => {{
        if $vm.get_slot_type($slot) != $crate::SlotType::Bool {
            panic!(
                "rust error [{}:{}]: Slot {} is not a <bool>",
                file!(),
                line!(),
                $slot
            )
        }
        $vm.get_slot_bool($slot).unwrap()
    }};

    ($vm:expr => string $slot:expr) => {{
        if $vm.get_slot_type($slot) != $crate::SlotType::String {
            panic!(
                "rust error [{}:{}]: Slot {} is not a <string>",
                file!(),
                line!(),
                $slot
            )
        }
        $vm.get_slot_string($slot).unwrap()
    }};

    ($vm:expr => bytes $slot:expr) => {{
        if $vm.get_slot_type($slot) != $crate::SlotType::String {
            panic!(
                "rust error [{}:{}]: Slot {} is not a <string>",
                file!(),
                line!(),
                $slot
            )
        }
        $vm.get_slot_bytes($slot).unwrap()
    }};

    ($vm:expr => foreign $t:ty => $slot:expr) => {{
        if $vm.get_slot_type($slot) != $crate::SlotType::Foreign {
            panic!(
                "rust error [{}:{}]: Slot {} is not a <foreign>",
                file!(),
                line!(),
                $slot
            )
        }
        match $vm.get_slot_foreign::<$t>($slot) {
            Some(ty) => ty,
            None => panic!(
                "rust error [{}:{}]: Slot {} is not a foreign of type {}",
                file!(),
                line!(),
                $slot,
                std::any::type_name::<$t>()
            ),
        }
    }};

    ($vm:expr => foreign_mut $t:ty => $slot:expr) => {{
        if $vm.get_slot_type($slot) != $crate::SlotType::Foreign {
            panic!(
                "rust error [{}:{}]: Slot {} is not a <foreign>",
                file!(),
                line!(),
                $slot
            )
        }
        match $vm.get_slot_foreign_mut::<$t>($slot) {
            Some(ty) => ty,
            None => panic!(
                "rust error [{}:{}]: Slot {} is not a foreign of type {}",
                file!(),
                line!(),
                $slot,
                std::any::type_name::<$t>()
            ),
        }
    }};
}
