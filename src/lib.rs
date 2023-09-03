//! We need to expose the Wren API in a Rust-y way
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::sync::mpsc::{channel, Receiver, Sender};
use wren_sys::{wrenGetUserData, WrenConfiguration, WrenHandle, WrenVM};

pub use wren_sys;

mod module_loader;
pub use module_loader::{BasicFileLoader, NullLoader};

use std::{any, ffi, marker, mem, os::raw};

mod runtime;
#[cfg(test)]
mod tests;

#[derive(Debug)]
/// Directly internally to report errors
pub enum WrenError {
    Compile(String, i32, String),
    Runtime(String),
    StackTrace(String, i32, String),
}

#[derive(Debug, Clone)]
/// Possible errors for a Wren script
pub enum VMError {
    Compile {
        module: String,
        line: i32,
        error: String,
    },
    Runtime {
        error: String,
        frames: Vec<VMStackFrameError>,
    },
}

#[derive(Debug, Clone)]
pub struct VMStackFrameError {
    pub module: String,
    pub line: i32,
    pub function: String,
}

impl std::fmt::Display for VMError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VMError::Compile {
                module,
                line,
                error,
            } => write!(fmt, "Compile Error ({}:{}): {}", module, line, error),
            VMError::Runtime { error, frames } => {
                writeln!(fmt, "Runtime Error: {}", error)?;
                for frame in frames {
                    if frame.function == "" {
                        writeln!(fmt, "\tin {}:{}: <constructor>", frame.module, frame.line)?;
                    } else {
                        writeln!(
                            fmt,
                            "\tin {}:{}: {}",
                            frame.module, frame.line, frame.function
                        )?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for VMError {}

/// A handle to a Wren object
#[derive(Debug, PartialEq, Eq)]
pub struct Handle<'a> {
    handle: *mut WrenHandle,
    wvm: *mut WrenVM,
    vm: marker::PhantomData<&'a VM>,
}

impl<'a> Drop for Handle<'a> {
    fn drop(&mut self) {
        unsafe {
            wren_sys::wrenReleaseHandle(self.wvm, self.handle);
        }
    }
}

/// A handle to a Wren method call
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionHandle<'a>(Handle<'a>);

/// Simulates a module structure for foreign functions
#[derive(Debug, Clone, Default)]
pub struct ModuleLibrary {
    modules: HashMap<String, Module>,
}

impl ModuleLibrary {
    /// Creates a new library
    pub fn new() -> ModuleLibrary {
        ModuleLibrary {
            modules: HashMap::new(),
        }
    }

    /// Adds a [`Module`] with a specified `name`
    pub fn module<N: Into<String>>(&mut self, name: N, modl: Module) {
        self.modules.insert(name.into(), modl);
    }

    /// Attempts to find a [`RuntimeClass`] given a `module` name and a `class` name
    fn get_foreign_class<M: AsRef<str>, C: AsRef<str>>(
        &self, module: M, class: C,
    ) -> Option<&RuntimeClass> {
        self.modules
            .get(module.as_ref())
            .and_then(|md| md.classes.get(class.as_ref()))
    }
}

#[derive(Debug, Clone)]
/// Represetnation of classes at runtime
struct RuntimeClass {
    construct: extern "C" fn(*mut WrenVM),
    destruct: extern "C" fn(*mut ffi::c_void),
    methods: ClassObjectPointers,

    // Use for "loading in" appropriate objects
    type_id: any::TypeId,
}

#[derive(Debug, Clone, Default)]
/// A container for `RuntimeClass` structs
pub struct Module {
    classes: HashMap<String, RuntimeClass>,
}

#[derive(Debug, Clone)]
/// List of [`MethodPointer`]s that make up the methods of a ['RuntimeClass`]
pub struct ClassObjectPointers {
    pub function_pointers: Vec<MethodPointer>,
}

#[derive(Debug, Clone)]
pub struct MethodPointer {
    pub is_static: bool,
    pub signature: FunctionSignature,
    pub pointer: unsafe extern "C" fn(*mut WrenVM),
}

impl Module {
    /// Create a new module
    pub fn new() -> Module {
        Module {
            classes: HashMap::new(),
        }
    }

    /// Add class `C` to this module with a `name`
    pub fn class<C: 'static + ClassObject, S: Into<String>>(&mut self, name: S) -> &mut Self {
        let cp = C::generate_pointers();
        let init = C::initialize_pointer();
        let deinit = C::finalize_pointer();
        self.classes.insert(
            name.into(),
            RuntimeClass {
                construct: init,
                destruct: deinit,
                methods: cp,
                type_id: any::TypeId::of::<C>(),
            },
        );
        self
    }
}

/// Initialize function for Wren classes
pub trait Class {
    fn initialize(_: &VM) -> Self
    where
        Self: Sized;
}

/// Indicates a "real" Wren class, and must be implemented to be added to a [`Module`]
pub trait ClassObject: Class {
    fn initialize_pointer() -> extern "C" fn(*mut WrenVM)
    where
        Self: Sized;
    fn finalize_pointer() -> extern "C" fn(*mut ffi::c_void)
    where
        Self: Sized;
    fn generate_pointers() -> ClassObjectPointers
    where
        Self: Sized;
}

#[derive(Debug, Copy, Clone)]
/// Indicates a "foreign object" to Wren
pub struct ForeignObject<T> {
    pub object: *mut T,
    pub type_id: any::TypeId,
}

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
                        let wptr = $crate::wren_sys::wrenSetSlotNewForeign(vm.borrow().vm, 0, 0, std::mem::size_of::<$crate::ForeignObject<$name>>() as $crate::wren_sys::size_t);
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

        mod $modl {
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

pub fn type_name_of<T>(_: &T) -> &'static str {
    any::type_name::<T>()
}

/// Sends a foreign object `$obj` as an object of `$class` in module `$modl` to slot `$slot`
#[macro_export]
macro_rules! send_foreign {
    ($vm:expr, $modl:expr, $class:expr, $obj:expr => $slot:expr) => {{
        let obj_name = $crate::type_name_of(&$obj);
        match $vm.set_slot_new_foreign($modl, $class, $obj, $slot) {
            Err(e) => panic!(
                "rust error [{}:{}]: Could not send type {:?} as [{}] {}: {}",
                file!(),
                line!(),
                obj_name,
                $modl,
                $class,
                e
            ),
            Ok(rf) => rf,
        }
    }};
}

/// Enables one to enable module loading for Wren
pub trait ModuleScriptLoader {
    /// Takes a desired module `name`
    ///
    /// ### Returns
    /// - `Some(String)` containing the Wren source if the module exists
    /// - `None` if not
    fn load_script(&mut self, name: String) -> Option<String>;
}

impl<T> ModuleScriptLoader for T
where
    T: FnMut(String) -> Option<String>,
{
    fn load_script(&mut self, name: String) -> Option<String> {
        (*self)(name)
    }
}

type EVM = Rc<RefCell<VM>>;

/// Sends strings for printing to an output
pub trait Printer {
    /// Called whenever a string is to be sent to output
    fn print(&mut self, s: String);
}

impl<T> Printer for T
where
    T: FnMut(String),
{
    fn print(&mut self, s: String) {
        (*self)(s)
    }
}

struct PrintlnPrinter;
impl Printer for PrintlnPrinter {
    fn print(&mut self, s: String) {
        print!("{}", s);
    }
}

#[derive(Debug)]
pub struct VM {
    pub vm: *mut WrenVM,
    error_recv: Receiver<WrenError>,
}

/// A mostly internal class that is exposed so that some externally generated code can access it.
pub struct UserData {
    error_channel: Sender<WrenError>,
    printer: Box<dyn Printer>,
    pub vm: Weak<RefCell<VM>>, // is used a *lot* by externally generated code.
    library: Option<ModuleLibrary>,
    loader: Box<dyn ModuleScriptLoader>,
}

/// Represents Wren slot types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotType {
    Num,
    Bool,
    List,
    Map,
    Null,
    String,
    Foreign,
    Unknown,
}

pub type SlotId = usize;

/// Represents Wren function signatures
#[derive(Debug, Clone)]
pub enum FunctionSignature {
    Function { name: String, arity: usize },
    Getter(String),
    Setter(String),
}

impl FunctionSignature {
    pub fn new_function<N: Into<String>>(name: N, arity: usize) -> FunctionSignature {
        FunctionSignature::Function {
            name: name.into(),
            arity,
        }
    }

    pub fn new_getter<N: Into<String>>(name: N) -> FunctionSignature {
        FunctionSignature::Getter(name.into())
    }

    pub fn new_setter<N: Into<String>>(name: N) -> FunctionSignature {
        FunctionSignature::Setter(name.into())
    }

    fn as_wren_string(&self) -> String {
        match self {
            FunctionSignature::Function { name, arity } => {
                format!("{}({})", name, vec!["_".to_string(); *arity].join(","))
            }
            FunctionSignature::Getter(name) => name.clone(),
            FunctionSignature::Setter(name) => format!("{}=(_)", name),
        }
    }

    /// Get number of arguments this function signature would require
    pub fn arity(&self) -> usize {
        match self {
            FunctionSignature::Function { arity, .. } => *arity,
            FunctionSignature::Getter(_) => 0,
            FunctionSignature::Setter(_) => 1,
        }
    }
}

/// High-level wrapper around a Wren VM
#[derive(Debug, Clone)]
pub struct VMWrapper(EVM);

impl VMWrapper {
    /// Calls a given function from its signature
    pub fn call(&self, signature: FunctionSignature) -> Result<(), VMError> {
        let handle = self.make_call_handle(signature);
        self.call_handle(&handle)
    }

    /// Calls a given function from its handle
    pub fn call_handle(&self, handle: &FunctionHandle) -> Result<(), VMError> {
        let vm = self.0.borrow();
        match unsafe { wren_sys::wrenCall(vm.vm, handle.0.handle) } {
            wren_sys::WrenInterpretResult_WREN_RESULT_SUCCESS => Ok(()),
            wren_sys::WrenInterpretResult_WREN_RESULT_COMPILE_ERROR => {
                unreachable!("wrenCall doesn't compile anything")
            }
            wren_sys::WrenInterpretResult_WREN_RESULT_RUNTIME_ERROR => {
                let mut error = "".to_string();
                let mut frames = vec![];
                while let Ok(err) = vm.error_recv.try_recv() {
                    match err {
                        WrenError::Runtime(msg) => {
                            error = msg;
                        }
                        WrenError::StackTrace(module, line, msg) => {
                            frames.push(VMStackFrameError {
                                module,
                                line,
                                function: msg,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                Err(VMError::Runtime { error, frames })
            }
            _ => unreachable!(),
        }
    }

    /// Interprets a given string as Wren code
    pub fn interpret<M: AsRef<str>, C: AsRef<str>>(
        &self, module: M, code: C,
    ) -> Result<(), VMError> {
        let module = ffi::CString::new(module.as_ref()).expect("module name conversion failed");
        let code = ffi::CString::new(code.as_ref()).expect("code conversion failed");
        let vm = self.0.borrow();
        match unsafe {
            wren_sys::wrenInterpret(
                vm.vm,
                module.as_ptr() as *const i8,
                code.as_ptr() as *const i8,
            )
        } {
            wren_sys::WrenInterpretResult_WREN_RESULT_SUCCESS => Ok(()),
            wren_sys::WrenInterpretResult_WREN_RESULT_COMPILE_ERROR => {
                match vm.error_recv.try_recv() {
                    Ok(WrenError::Compile(module, line, msg)) => Err(VMError::Compile {
                        module,
                        line,
                        error: msg,
                    }),
                    _ => unreachable!(),
                }
            }
            wren_sys::WrenInterpretResult_WREN_RESULT_RUNTIME_ERROR => {
                let mut error = "".to_string();
                let mut frames = vec![];
                while let Ok(err) = vm.error_recv.try_recv() {
                    match err {
                        WrenError::Runtime(msg) => {
                            error = msg;
                        }
                        WrenError::StackTrace(module, line, msg) => {
                            frames.push(VMStackFrameError {
                                module,
                                line,
                                function: msg,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                Err(VMError::Runtime { error, frames })
            }
            _ => unreachable!(),
        }
    }

    /// Allows access to the internal VM wrapper object
    pub fn execute<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&VM) -> T,
    {
        f(&self.0.borrow())
    }

    /// Gets a handle to a value in a certain slot
    pub fn get_slot_handle(&self, slot: SlotId) -> Rc<Handle> {
        Rc::new(Handle {
            handle: unsafe { wren_sys::wrenGetSlotHandle(self.0.borrow().vm, slot as raw::c_int) },
            wvm: self.0.borrow().vm,
            vm: marker::PhantomData,
        })
    }

    /// Sets the value in a certain slot to the value of a handle
    pub fn set_slot_handle(&self, slot: SlotId, handle: &Handle) {
        unsafe {
            wren_sys::wrenSetSlotHandle(self.0.borrow().vm, slot as raw::c_int, handle.handle)
        }
    }

    /// Create a callable handle, that can be used with [`call_handle`](VMWrapper::call_handle)
    pub fn make_call_handle(&self, signature: FunctionSignature) -> Rc<FunctionHandle> {
        VM::make_call_handle(self.0.borrow().vm, signature)
    }

    /// Instruct Wren to start a garbage collection cycle
    pub fn collect_garbage(&self) {
        unsafe { wren_sys::wrenCollectGarbage(self.0.borrow().vm) }
    }
}

/// Allows for the customization of a Wren VM
pub struct VMConfig {
    printer: Box<dyn Printer>,
    script_loader: Box<dyn ModuleScriptLoader>,
    library: Option<ModuleLibrary>,
    initial_heap_size: usize,
    min_heap_size: usize,
    heap_growth_percent: usize,

    /// Enables @module syntax to mean `module` loaded relative to current module
    enable_relative_import: bool,
}

impl Default for VMConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl VMConfig {
    pub fn new() -> VMConfig {
        VMConfig {
            printer: Box::new(PrintlnPrinter),
            script_loader: Box::new(NullLoader),
            library: None,
            initial_heap_size: 1024 * 1024 * 10,
            min_heap_size: 1024 * 1024,
            heap_growth_percent: 50,
            enable_relative_import: false,
        }
    }

    pub fn printer<P: 'static + Printer>(mut self, p: P) -> Self {
        self.printer = Box::new(p);
        self
    }

    pub fn script_loader<L: 'static + ModuleScriptLoader>(mut self, l: L) -> Self {
        self.script_loader = Box::new(l);
        self
    }

    pub fn library(mut self, l: &ModuleLibrary) -> Self {
        self.library = Some(l.clone());
        self
    }

    pub fn no_library(mut self) -> Self {
        self.library = None;
        self
    }

    pub fn initial_heap_size(mut self, ihs: usize) -> Self {
        self.initial_heap_size = ihs;
        self
    }

    pub fn min_heap_size(mut self, mhs: usize) -> Self {
        self.min_heap_size = mhs;
        self
    }

    pub fn heap_growth_percent(mut self, hgp: usize) -> Self {
        self.heap_growth_percent = hgp;
        self
    }

    pub fn enable_relative_import(mut self, eri: bool) -> Self {
        self.enable_relative_import = eri;
        self
    }

    pub fn build(self) -> VMWrapper {
        let (etx, erx) = channel();

        // Have an uninitialized VM...
        let wvm = Rc::new(RefCell::new(VM {
            vm: std::ptr::null_mut(),
            error_recv: erx,
        }));

        let vm_config = Box::into_raw(Box::new(UserData {
            error_channel: etx,
            printer: self.printer,
            vm: Rc::downgrade(&wvm),
            loader: self.script_loader,
            library: self.library,
        }));

        // Configure the Wren side of things
        let mut config = unsafe {
            let mut uconfig = mem::MaybeUninit::<WrenConfiguration>::zeroed();
            wren_sys::wrenInitConfiguration(uconfig.as_mut_ptr());
            let mut config = uconfig.assume_init();
            config.errorFn = Some(runtime::wren_error);
            config.writeFn = Some(runtime::wren_print);
            config.reallocateFn = Some(runtime::wren_realloc);
            config.bindForeignMethodFn = Some(runtime::wren_bind_foreign_method);
            config.bindForeignClassFn = Some(runtime::wren_bind_foreign_class);
            config.loadModuleFn = Some(runtime::wren_load_module);
            config.resolveModuleFn = if self.enable_relative_import {
                Some(runtime::wren_canonicalize)
            } else {
                None
            };
            config.initialHeapSize = self.initial_heap_size as wren_sys::size_t;
            config.minHeapSize = self.min_heap_size as wren_sys::size_t;
            config.heapGrowthPercent = self.heap_growth_percent as raw::c_int;
            config.userData = vm_config as *mut ffi::c_void;
            config
        };

        let vm = unsafe { wren_sys::wrenNewVM(&mut config) };
        wvm.borrow_mut().vm = vm;
        VMWrapper(wvm)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Errors that can happen when sending a foreign object to Wren
pub enum ForeignSendError {
    /// No ['RuntimeClass'] exists in the specfied module with the given name
    NoForeignClass,
    /// No Wrne declaration of the foreign class was made
    NoWrenClass,
    /// Ran out of memory to allocate the class
    NoMemory,
    /// The type of the ['RuntimeClass`] [`ClassObject`] differes from the given object
    ClassMismatch,
}

impl std::fmt::Display for ForeignSendError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ForeignSendError::NoForeignClass => write!(fmt, "no foreign class"),
            ForeignSendError::NoWrenClass => write!(fmt, "no Wren class"),
            ForeignSendError::NoMemory => write!(fmt, "unable to allocate memory"),
            ForeignSendError::ClassMismatch => write!(fmt, "class mismatch"),
        }
    }
}

impl std::error::Error for ForeignSendError {}

impl VM {
    // Slot and Handle API
    pub fn ensure_slots(&self, count: usize) {
        unsafe { wren_sys::wrenEnsureSlots(self.vm, count as raw::c_int) }
    }

    pub fn get_slot_count(&self) -> usize {
        unsafe { wren_sys::wrenGetSlotCount(self.vm) as usize }
    }

    pub fn set_slot_bool(&self, slot: SlotId, val: bool) {
        self.ensure_slots(slot + 1);
        unsafe { wren_sys::wrenSetSlotBool(self.vm, slot as raw::c_int, val) }
    }

    pub fn set_slot_double(&self, slot: SlotId, val: f64) {
        self.ensure_slots(slot + 1);
        unsafe { wren_sys::wrenSetSlotDouble(self.vm, slot as raw::c_int, val) }
    }

    pub fn set_slot_null(&self, slot: SlotId) {
        self.ensure_slots(slot + 1);
        unsafe { wren_sys::wrenSetSlotNull(self.vm, slot as raw::c_int) }
    }

    pub fn set_slot_bytes(&self, slot: SlotId, bytes: &[u8]) {
        self.ensure_slots(slot + 1);
        unsafe {
            wren_sys::wrenSetSlotBytes(
                self.vm,
                slot as raw::c_int,
                bytes as *const _ as *const raw::c_char,
                bytes.len() as wren_sys::size_t,
            );
        }
    }

    pub fn set_slot_string<S: AsRef<str>>(&self, slot: SlotId, string: S) {
        self.ensure_slots(slot + 1);
        let string = string.as_ref();
        unsafe {
            wren_sys::wrenSetSlotBytes(
                self.vm,
                slot as raw::c_int,
                string.as_ptr() as *const _,
                string.len() as wren_sys::size_t,
            );
        }
    }

    pub fn get_slot_bool(&self, slot: SlotId) -> Option<bool> {
        self.ensure_slots(slot + 1);
        if self.get_slot_type(slot) != SlotType::Bool {
            None
        } else {
            unsafe { Some(wren_sys::wrenGetSlotBool(self.vm, slot as raw::c_int)) }
        }
    }

    pub fn get_slot_double(&self, slot: SlotId) -> Option<f64> {
        self.ensure_slots(slot + 1);
        if self.get_slot_type(slot) != SlotType::Num {
            None
        } else {
            unsafe { Some(wren_sys::wrenGetSlotDouble(self.vm, slot as raw::c_int)) }
        }
    }

    pub fn get_slot_bytes(&self, slot: SlotId) -> Option<Vec<u8>> {
        self.ensure_slots(slot + 1);
        if self.get_slot_type(slot) != SlotType::String {
            None
        } else {
            let mut length = 0 as raw::c_int;
            let ptr = unsafe {
                wren_sys::wrenGetSlotBytes(self.vm, slot as raw::c_int, &mut length as *mut _)
            };
            let mut bytes = vec![];

            // Do some pointer maths to get the vector. Hurrah!
            for offset in 0..length {
                unsafe { bytes.push(*ptr.offset(offset as isize) as u8) }
            }

            Some(bytes)
        }
    }

    pub fn get_slot_string(&self, slot: SlotId) -> Option<String> {
        self.ensure_slots(slot + 1);
        if self.get_slot_type(slot) != SlotType::String {
            None
        } else {
            let ptr = unsafe { wren_sys::wrenGetSlotString(self.vm, slot as raw::c_int) };

            let cstr = unsafe { ffi::CStr::from_ptr(ptr) };

            Some(cstr.to_string_lossy().to_string())
        }
    }

    pub fn get_slot_type(&self, slot: SlotId) -> SlotType {
        self.ensure_slots(slot + 1);
        match unsafe { wren_sys::wrenGetSlotType(self.vm, slot as raw::c_int) } {
            wren_sys::WrenType_WREN_TYPE_NUM => SlotType::Num,
            wren_sys::WrenType_WREN_TYPE_BOOL => SlotType::Bool,
            wren_sys::WrenType_WREN_TYPE_LIST => SlotType::List,
            wren_sys::WrenType_WREN_TYPE_MAP => SlotType::Map,
            wren_sys::WrenType_WREN_TYPE_NULL => SlotType::Null,
            wren_sys::WrenType_WREN_TYPE_STRING => SlotType::String,
            wren_sys::WrenType_WREN_TYPE_FOREIGN => SlotType::Foreign,
            wren_sys::WrenType_WREN_TYPE_UNKNOWN => SlotType::Unknown,
            _ => unreachable!(),
        }
    }

    /// Returns Some(()) if the variable was found and stored in the given slot
    ///
    /// Returns None if the variable does not exist
    pub fn get_variable<M: AsRef<str>, N: AsRef<str>>(
        &self, module: M, name: N, slot: SlotId,
    ) -> bool {
        self.ensure_slots(slot + 1);
        if !self.has_variable(&module, &name) {
            return false;
        }
        let module = ffi::CString::new(module.as_ref()).expect("module name conversion failed");
        let name = ffi::CString::new(name.as_ref()).expect("variable name conversion failed");
        unsafe {
            wren_sys::wrenGetVariable(self.vm, module.as_ptr(), name.as_ptr(), slot as raw::c_int)
        }
        true
    }

    pub fn has_variable<M: AsRef<str>, N: AsRef<str>>(&self, module: M, name: N) -> bool {
        if !self.has_module(&module) {
            return false;
        }
        let module = ffi::CString::new(module.as_ref()).expect("module name conversion failed");
        let name = ffi::CString::new(name.as_ref()).expect("variable name conversion failed");
        unsafe { wren_sys::wrenHasVariable(self.vm, module.as_ptr(), name.as_ptr()) }
    }

    pub fn has_module<M: AsRef<str>>(&self, module: M) -> bool {
        let module = ffi::CString::new(module.as_ref()).expect("module name conversion failed");
        unsafe { wren_sys::wrenHasModule(self.vm, module.as_ptr()) }
    }

    pub fn set_slot_new_list(&self, slot: SlotId) {
        self.ensure_slots(slot + 1);
        unsafe { wren_sys::wrenSetSlotNewList(self.vm, slot as raw::c_int) }
    }

    pub fn get_list_count(&self, slot: SlotId) -> Option<usize> {
        self.ensure_slots(slot + 1);
        if self.get_slot_type(slot) == SlotType::List {
            Some(unsafe { wren_sys::wrenGetListCount(self.vm, slot as raw::c_int) as usize })
        } else {
            None
        }
    }

    pub fn insert_in_list(&self, list_slot: SlotId, index: i32, element_slot: SlotId) {
        self.ensure_slots(element_slot + 1);
        self.ensure_slots(list_slot + 1);
        unsafe {
            wren_sys::wrenInsertInList(
                self.vm,
                list_slot as raw::c_int,
                index as raw::c_int,
                element_slot as raw::c_int,
            )
        }
    }

    pub fn get_list_element(&self, list_slot: SlotId, index: i32, element_slot: SlotId) {
        self.ensure_slots(element_slot + 1);
        self.ensure_slots(list_slot + 1);
        unsafe {
            wren_sys::wrenGetListElement(
                self.vm,
                list_slot as raw::c_int,
                index as raw::c_int,
                element_slot as raw::c_int,
            )
        }
    }

    pub fn set_list_element(&self, list_slot: SlotId, index: i32, element_slot: SlotId) {
        self.ensure_slots(element_slot + 1);
        self.ensure_slots(list_slot + 1);
        unsafe {
            wren_sys::wrenSetListElement(
                self.vm,
                list_slot as raw::c_int,
                index as raw::c_int,
                element_slot as raw::c_int,
            )
        }
    }

    pub fn set_slot_new_map(&self, slot: SlotId) {
        self.ensure_slots(slot + 1);
        unsafe { wren_sys::wrenSetSlotNewMap(self.vm, slot as raw::c_int) }
    }

    pub fn get_map_count(&self, slot: SlotId) -> Option<usize> {
        self.ensure_slots(slot + 1);
        if self.get_slot_type(slot) == SlotType::Map {
            Some(unsafe { wren_sys::wrenGetMapCount(self.vm, slot as raw::c_int) as usize })
        } else {
            None
        }
    }

    pub fn get_map_contains_key(&self, map_slot: SlotId, key_slot: SlotId) -> Option<bool> {
        self.ensure_slots(map_slot + 1);
        self.ensure_slots(key_slot + 1);
        if self.get_slot_type(map_slot) == SlotType::Map {
            Some(unsafe {
                wren_sys::wrenGetMapContainsKey(
                    self.vm,
                    map_slot as raw::c_int,
                    key_slot as raw::c_int,
                )
            })
        } else {
            None
        }
    }

    pub fn get_map_value(&self, map_slot: SlotId, key_slot: SlotId, value_slot: SlotId) {
        self.ensure_slots(map_slot + 1);
        self.ensure_slots(key_slot + 1);
        self.ensure_slots(value_slot + 1);
        unsafe {
            wren_sys::wrenGetMapValue(
                self.vm,
                map_slot as raw::c_int,
                key_slot as raw::c_int,
                value_slot as raw::c_int,
            )
        }
    }

    pub fn set_map_value(&self, map_slot: SlotId, key_slot: SlotId, value_slot: SlotId) {
        self.ensure_slots(map_slot + 1);
        self.ensure_slots(key_slot + 1);
        self.ensure_slots(value_slot + 1);
        unsafe {
            wren_sys::wrenSetMapValue(
                self.vm,
                map_slot as raw::c_int,
                key_slot as raw::c_int,
                value_slot as raw::c_int,
            )
        }
    }

    pub fn remove_map_value(&self, map_slot: SlotId, key_slot: SlotId, removed_value_slot: SlotId) {
        self.ensure_slots(map_slot + 1);
        self.ensure_slots(key_slot + 1);
        self.ensure_slots(removed_value_slot + 1);
        unsafe {
            wren_sys::wrenRemoveMapValue(
                self.vm,
                map_slot as raw::c_int,
                key_slot as raw::c_int,
                removed_value_slot as raw::c_int,
            )
        }
    }

    pub fn get_slot_foreign<T: 'static + ClassObject>(&self, slot: SlotId) -> Option<&T> {
        self.ensure_slots(slot + 1);
        self.get_slot_foreign_mut(slot).map(|mr| &*mr)
    }

    pub fn get_slot_foreign_mut<T: 'static + ClassObject>(&self, slot: SlotId) -> Option<&mut T> {
        self.ensure_slots(slot + 1);
        unsafe {
            let ptr = wren_sys::wrenGetSlotForeign(self.vm, slot as raw::c_int);
            if !ptr.is_null() {
                let fo = std::ptr::read_unaligned(ptr as *mut ForeignObject<T>);
                let ret = if fo.type_id == any::TypeId::of::<T>() {
                    // Safe to downcast
                    fo.object.as_mut()
                } else {
                    // Incorrect type, unsafe to downcast
                    None
                };
                std::ptr::write_unaligned(ptr as *mut ForeignObject<T>, fo);
                ret
            } else {
                None
            }
        }
    }

    /// Looks up the specified module for the given class
    /// If it's type matches with type T, will create a new instance in the given slot
    ///  
    /// WARNING: This *will* overwrite slot 0, so be careful.
    pub fn set_slot_new_foreign<M: AsRef<str>, C: AsRef<str>, T: 'static + ClassObject>(
        &self, module: M, class: C, object: T, slot: SlotId,
    ) -> Result<&mut T, ForeignSendError> {
        self.set_slot_new_foreign_scratch(module, class, object, slot, 0)
    }

    /// Looks up the specified module for the given class
    /// If it's type matches with type T, will create a new instance in the given slot
    ///  
    /// WARNING: This *will* overwrite slot `scratch`, so be careful.
    pub fn set_slot_new_foreign_scratch<M: AsRef<str>, C: AsRef<str>, T: 'static + ClassObject>(
        &self, module: M, class: C, object: T, slot: SlotId, scratch: SlotId,
    ) -> Result<&mut T, ForeignSendError> {
        self.ensure_slots(slot.max(scratch) + 1);
        let conf = unsafe {
            std::ptr::read_unaligned(wren_sys::wrenGetUserData(self.vm) as *mut UserData)
        };

        // Why did I put this here? (well the equivalent in the original method...)
        self.ensure_slots((slot.max(scratch) + 1) as usize);
        // Even if slot == 0, we can just load the class into slot 0, then use wrenSetSlotNewForeign to "create" a new object
        let ret = match conf
            .library
            .as_ref()
            .and_then(|lib| lib.get_foreign_class(module.as_ref(), class.as_ref()))
        {
            None => Err(ForeignSendError::NoForeignClass), // Couldn't find the corresponding class
            Some(runtime_class) => {
                if runtime_class.type_id == any::TypeId::of::<T>() {
                    // The Wren foreign class corresponds with this real object.
                    // We can coerce it and treat this object as that class, even if not instantiated by Wren.

                    // Create the new ForeignObject
                    let new_obj = ForeignObject {
                        object: Box::into_raw(Box::new(object)),
                        type_id: any::TypeId::of::<T>(),
                    };

                    // Load the Wren class object into scratch slot.
                    self.get_variable(module, class, scratch);

                    // Make sure the class isn't null (undeclared in Wren code)
                    match self.get_slot_type(scratch) {
                        SlotType::Null => Err(ForeignSendError::NoWrenClass), // You haven't declared the foreign class to Wren
                        SlotType::Unknown => unsafe {
                            // A Wren class
                            // Create the Wren foreign pointer
                            let wptr = wren_sys::wrenSetSlotNewForeign(
                                self.vm,
                                slot as raw::c_int,
                                scratch as raw::c_int,
                                mem::size_of::<ForeignObject<T>>() as wren_sys::size_t,
                            );

                            if !wptr.is_null() {
                                // Move the ForeignObject into the pointer
                                std::ptr::write(wptr as *mut _, new_obj);
                            }

                            // Reinterpret the pointer as an object if we were successful
                            match (wptr as *mut ForeignObject<T>).as_mut() {
                                Some(ptr) => Ok(ptr.object.as_mut().unwrap()),
                                None => Err(ForeignSendError::NoMemory),
                            }
                        },
                        _ => Err(ForeignSendError::NoWrenClass),
                    }
                } else {
                    // The classes do not match. Avoid.
                    Err(ForeignSendError::ClassMismatch)
                }
            }
        };

        unsafe {
            std::ptr::write_unaligned(wrenGetUserData(self.vm) as *mut UserData, conf);
        }
        ret
    }

    fn make_call_handle<'b>(
        vm: *mut WrenVM, signature: FunctionSignature,
    ) -> Rc<FunctionHandle<'b>> {
        let signature =
            ffi::CString::new(signature.as_wren_string()).expect("signature conversion failed");
        Rc::new(FunctionHandle(Handle {
            handle: unsafe { wren_sys::wrenMakeCallHandle(vm, signature.as_ptr()) },
            wvm: vm,
            vm: marker::PhantomData,
        }))
    }

    pub fn abort_fiber(&self, slot: SlotId) {
        unsafe { wren_sys::wrenAbortFiber(self.vm, slot as raw::c_int) }
    }

    pub fn get_version_number(&self) -> i32 {
        unsafe { wren_sys::wrenGetVersionNumber() }
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        unsafe {
            let conf = wren_sys::wrenGetUserData(self.vm);
            let _: Box<UserData> = Box::from_raw(conf as *mut _); // Drop the userdata
            wren_sys::wrenFreeVM(self.vm);
        }
    }
}
