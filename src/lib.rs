//! We need to expose the Wren API in a Rust-y way
use wren_sys::{WrenVM, WrenHandle, WrenConfiguration, WrenErrorType, WrenForeignClassMethods};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

use std::{mem, ffi, os::raw, any};

#[derive(Debug)]
pub enum WrenError {
    Compile(String, i32, String),
    Runtime(String),
    StackTrace(String, i32, String),
}

// Force Wren to use Rust's allocator to allocate memory
extern "C" fn wren_realloc(memory: *mut ffi::c_void, new_size: wren_sys::size_t) -> *mut ffi::c_void {
    unsafe {
        if memory == std::ptr::null_mut() { // If memory == NULL
            // allocate new memory
            std::alloc::alloc_zeroed(std::alloc::Layout::from_size_align(new_size as usize, 8).unwrap()) as *mut _
        } else {
            // Memory is an actual pointer to a location.
            if new_size == 0 {
                std::alloc::dealloc(memory as *mut _, std::alloc::Layout::from_size_align(0, 8).unwrap());
                std::ptr::null_mut()
            } else {
                std::alloc::realloc(memory as *mut _, std::alloc::Layout::from_size_align(new_size as usize, 8).unwrap(), new_size as usize) as *mut _
            }
        }
    }
}

extern "C" fn wren_error(vm: *mut WrenVM, typ: WrenErrorType, module: *const raw::c_char, line: raw::c_int, message: *const raw::c_char) {
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    match typ {
        wren_sys::WrenErrorType_WREN_ERROR_COMPILE => {
            let module_str = unsafe { ffi::CStr::from_ptr(module) };
            let message_str = unsafe { ffi::CStr::from_ptr(message) };
            conf.error_channel.send(WrenError::Compile(module_str.to_string_lossy().to_string(), line as i32, message_str.to_string_lossy().to_string())).unwrap();
        },
        wren_sys::WrenErrorType_WREN_ERROR_RUNTIME => {
            let message_str = unsafe { ffi::CStr::from_ptr(message) };
            conf.error_channel.send(WrenError::Runtime(message_str.to_string_lossy().to_string())).unwrap();
        },
        wren_sys::WrenErrorType_WREN_ERROR_STACK_TRACE => {
            let module_str = unsafe { ffi::CStr::from_ptr(module) };
            let message_str = unsafe { ffi::CStr::from_ptr(message) };
            conf.error_channel.send(WrenError::StackTrace(module_str.to_string_lossy().to_string(), line as i32, message_str.to_string_lossy().to_string())).unwrap();
        },
        _ => unreachable!()
    }
}

extern "C" fn wren_print(vm: *mut WrenVM, message: *const raw::c_char) {
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let message_str = unsafe { ffi::CStr::from_ptr(message) };
    conf.printer.print(message_str.to_string_lossy().to_string());
}

extern "C" fn wren_bind_foreign_method(vm: *mut WrenVM, mdl: *const raw::c_char, class: *const raw::c_char, is_static: bool, sgn: *const raw::c_char) -> Option<unsafe extern "C" fn(*mut WrenVM)> {
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let module = unsafe { ffi::CStr::from_ptr(mdl) };
    let class = unsafe { ffi::CStr::from_ptr(class) };
    let signature = unsafe { ffi::CStr::from_ptr(sgn) };

    if let Some(library) = conf.library {
        if let Some(rc) = library.get_foreign_class(module.to_string_lossy(), class.to_string_lossy()) {
            rc.methods.function_pointers.iter().find(|mp| {
                if mp.signature == signature.to_string_lossy() && mp.is_static == is_static {
                    true
                } else {
                    false
                }
            }).map(|mp| mp.pointer)
        } else {
            None
        }
    } else {
        None
    }
}

extern "C" fn wren_bind_foreign_class(vm: *mut WrenVM, mdl: *const raw::c_char, class: *const raw::c_char) -> WrenForeignClassMethods {
    let mut fcm = WrenForeignClassMethods {
        allocate: None,
        finalize: None
    };

    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let module = unsafe { ffi::CStr::from_ptr(mdl) };
    let class = unsafe { ffi::CStr::from_ptr(class) };

    if let Some(library) = conf.library {
        let rc = library.get_foreign_class(module.to_string_lossy(), class.to_string_lossy());
        if let Some(rc) = rc {
            fcm.allocate = Some(rc.construct);
            fcm.finalize = Some(rc.destruct);
        }
    }
    fcm
}

extern "C" fn wren_load_module(vm: *mut WrenVM, name: *const raw::c_char) -> *mut raw::c_char {
    // The whoooole reason we wrote wren_realloc - to force Wren into Rust's allocation space
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let module_name = unsafe { ffi::CStr::from_ptr(name) };
    match conf.loader.load_script(module_name.to_string_lossy().to_string()) {
        Some(string) => {
            ffi::CString::new(string).ok().map(|strg| strg.into_raw()).expect(&format!("Failed to convert source to C string for {}", module_name.to_string_lossy()))
        },
        None => std::ptr::null_mut()
    }
}

#[derive(Debug, Clone)]
pub enum VMError {
    Compile {
        module: String,
        line: i32,
        error: String
    },
    Runtime {
        error: String,
        frames: Vec<VMStackFrameError>
    }
}

#[derive(Debug, Clone)]
pub struct VMStackFrameError {
    module: String,
    line: i32,
    function: String
}


impl std::fmt::Display for VMError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VMError::Compile { module, line, error } => write!(fmt, "Compile Error ({}:{}): {}", module, line, error),
            VMError::Runtime { error, frames } => {
                writeln!(fmt, "Runtime Error: {}", error)?;
                for frame in frames {
                    if frame.function == "" {
                        writeln!(fmt, "\tin {}:{}: <constructor>", frame.module, frame.line)?;
                    } else {
                        writeln!(fmt, "\tin {}:{}: {}", frame.module, frame.line, frame.function)?;
                    }
                }
                Ok(())
            },
        }
    }
}

impl std::error::Error for VMError {}

pub struct Handle<'a> {
    handle: *mut WrenHandle,
    vm: &'a VM<'a>
}

impl<'a> Drop for Handle<'a> {
    fn drop(&mut self) {
        unsafe {
            wren_sys::wrenReleaseHandle(self.vm.vm, self.handle);
        }
    }
}

/// Simulates a module structure for foreign functions
#[derive(Debug)]
pub struct ModuleLibrary {
    modules: HashMap<String, Module>,
}

impl ModuleLibrary {
    pub fn new() -> ModuleLibrary {
        ModuleLibrary {
            modules: HashMap::new()
        }
    }

    pub fn module<N: AsRef<str>>(&mut self, name: N, modl: Module) {
        self.modules.insert(name.as_ref().to_string(), modl);
    }

    fn get_foreign_class<M: AsRef<str>, C: AsRef<str>>(&self, module: M, class: C) -> Option<&RuntimeClass> {
        self.modules.get(module.as_ref()).and_then(|md| md.classes.get(class.as_ref()))
    }
}

#[derive(Debug)]
struct RuntimeClass {
    construct: extern "C" fn(*mut WrenVM),
    destruct: extern "C" fn(*mut ffi::c_void),
    methods: ClassObjectPointers,

    // Use for "loading in" appropriate objects
    type_id: any::TypeId,
}

#[derive(Debug)]
pub struct Module {
    classes: HashMap<String, RuntimeClass>,
}

#[derive(Debug)]
pub struct ClassObjectPointers {
    pub function_pointers: Vec<MethodPointer>,
}

#[derive(Debug)]
pub struct MethodPointer {
    pub is_static: bool,
    pub signature: String,
    pub pointer: unsafe extern "C" fn(*mut WrenVM),
}

impl Module {
    pub fn new() -> Module {
        Module {
            classes: HashMap::new()
        }
    }

    pub fn class<C: 'static + ClassObject, S: AsRef<str>>(&mut self, name: S) -> &mut Self {
        let cp = C::generate_pointers();
        let init = C::initialize_pointer();
        let deinit = C::finalize_pointer();
        self.classes.insert(name.as_ref().to_string(), RuntimeClass {
            construct: init,
            destruct: deinit,
            methods: cp,
            type_id: any::TypeId::of::<C>(),
        });
        self
    }
}

// Trait that all Wren "class" objects implement
pub trait Class {
    fn initialize(_: &VM) -> Self;
}

pub trait ClassObject: Class {
    fn initialize_pointer() -> extern "C" fn(*mut WrenVM);
    fn finalize_pointer() -> extern "C" fn(*mut ffi::c_void);
    fn generate_pointers() -> ClassObjectPointers;
}

pub struct ForeignObject<T> {
    pub object: *mut T,
    pub type_id: any::TypeId,
}

/// Creates a function at $modl::publish_module, that takes a &mut ModuleLibrary
/// and handles Module object creation and registration
/// 
/// Also internally creates all the necessary extern "C" functions for Wren's callbacks
#[macro_export]
macro_rules! create_class_objects {
    (
        $(
            class($mname:expr) $name:ty => $md:ident {
                $(
                    static($sgns:expr) $sf:ident 
                ),*
                $(
                    instance($sgni:expr) $inf:ident
                ),*
            }
        )+

        module => $modl:ident
    ) => {
        $(
            mod $md {
                use std::panic::{take_hook, set_hook, catch_unwind, AssertUnwindSafe};
                pub(in super) extern "C" fn _constructor(vm: *mut wren_sys::WrenVM) {
                    use $crate::Class;
                    unsafe {
                        let conf = &mut *(wren_sys::wrenGetUserData(vm) as *mut $crate::UserData);
                        let vm = std::rc::Weak::upgrade(&conf.vm).expect(&format!("Failed to access VM at {:p}", &conf.vm));
                        let wptr = wren_sys::wrenSetSlotNewForeign(vm.borrow().vm, 0, 0, std::mem::size_of::<$crate::ForeignObject<$name>>() as wren_sys::size_t);
                        // Allocate a new object, and move it onto the heap
                        // TODO Include panic -> runtime error
                        set_hook(Box::new(|_| {}));
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
                        if let Some(object) = object {
                            let new_obj = Box::new($crate::ForeignObject {
                                object: Box::into_raw(Box::new(object)),
                                type_id: std::any::TypeId::of::<$name>(),
                            });
                            std::ptr::copy_nonoverlapping(Box::leak(new_obj), wptr as *mut _, 1);
                        }
                    }
                }

                pub(in super) extern "C" fn _destructor(data: *mut std::ffi::c_void) {
                    unsafe {
                        let fo: &mut $crate::ForeignObject<$name> = &mut *(data as *mut $crate::ForeignObject<$name>);
                        drop(Box::from_raw(fo.object));
                        fo.object = std::ptr::null_mut();
                    }
                }

                $(
                    $crate::create_class_objects!(@fn static $name => $sf);
                )*

                $(
                    $crate::create_class_objects!(@fn instance $name => $inf);
                )*
            }

            impl $crate::ClassObject for $name {
                fn initialize_pointer() -> extern "C" fn(*mut wren_sys::WrenVM) { $md::_constructor }
                fn finalize_pointer() -> extern "C" fn(*mut std::ffi::c_void) { $md::_destructor }
                fn generate_pointers() -> $crate::ClassObjectPointers {
                    $crate::ClassObjectPointers {
                        function_pointers: vec![
                            $(
                                $crate::MethodPointer {
                                    pointer: $md::$sf,
                                    signature: $sgns.into(),
                                    is_static: true,
                                }
                            ),*
                            $(
                                $crate::MethodPointer {
                                    pointer: $md::$inf,
                                    signature: $sgni.into(),
                                    is_static: false,
                                }
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
                lib.module(stringify!($modl), module);
            }
        }
    };

    (@fn static $name:ty => $s:ident) => {
        pub(in super) unsafe extern "C" fn $s(vm: *mut wren_sys::WrenVM) {
            use std::panic::{take_hook, set_hook, catch_unwind, AssertUnwindSafe};

            let conf = &mut *(wren_sys::wrenGetUserData(vm) as *mut $crate::UserData);
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
        }
    };

    (@fn instance $name:ty => $inf:ident) => {
        pub(in super) unsafe extern "C" fn $inf(vm: *mut wren_sys::WrenVM) {
            use std::panic::{take_hook, set_hook, catch_unwind, AssertUnwindSafe};
            
            let conf = &mut *(wren_sys::wrenGetUserData(vm) as *mut $crate::UserData);
            let vm = std::rc::Weak::upgrade(&conf.vm).expect(&format!("Failed to access VM at {:p}", &conf.vm));
            set_hook(Box::new(|_| {}));
            let vm_borrow = AssertUnwindSafe(vm.borrow());
            match catch_unwind(|| {
                vm_borrow.ensure_slots(1);
                let inst = vm_borrow.get_slot_foreign_mut::<$name>(0)
                    .expect(&format!("Tried to call {0} of {1:?} on non-{1:?} type", stringify!($inf), std::any::TypeId::of::<$name>()));
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
        }
    }
}

/// Enables one to plug-in a module loader for Wren
pub trait ModuleScriptLoader {
    fn load_script(&mut self, name: String) -> Option<String>;
}

pub type EVM<'a> = Rc<RefCell<VM<'a>>>;

pub trait Executor {
    fn execute<F>(&self, function: F) where F: FnMut(&VM);
}

impl<'a> Executor for EVM<'a> {
    fn execute<F>(&self, mut function: F) where F: FnMut(&VM) {
        function(&*self.borrow())
    }
}

pub trait Printer {
    fn print(&mut self, s: String);
}

pub struct PrintlnPrinter;
impl Printer for PrintlnPrinter {
    fn print(&mut self, s: String) {
        print!("{}", s);
    }
}

pub struct NullLoader;
impl ModuleScriptLoader for NullLoader {
    fn load_script(&mut self, _: String) -> Option<String> { None }
}

pub struct VM<'l> {
    pub vm: *mut WrenVM,
    error_recv: Receiver<WrenError>,
    module: std::marker::PhantomData<&'l ModuleLibrary>
}

/// A mostly internal class that is exposed so that some externally generated code can access it.
pub struct UserData<'a> {
    error_channel: Sender<WrenError>,
    printer: Box<dyn Printer>,
    pub vm: Weak<RefCell<VM<'a>>>, // is used a *lot* by externally generated code.
    library: Option<&'a ModuleLibrary>,
    loader: Box<dyn ModuleScriptLoader>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotType {
    Num,
    Bool,
    List,
    Null,
    String,
    Foreign,
    Unknown
}

// TODO Expose more VM configuration (initalHeap, maxHeap, etc.)
impl<'a> VM<'a> {
    pub fn new_terminal(library: Option<&ModuleLibrary>) -> EVM {
        VM::new(PrintlnPrinter, NullLoader, library)
    }

    pub fn new<P: 'static + Printer, L: 'static + ModuleScriptLoader>(p: P, l: L, library: Option<&ModuleLibrary>) -> EVM {
        let (etx, erx) = channel();

        // Have an uninitialized VM...
        let wvm = Rc::new(RefCell::new(VM {
            vm: std::ptr::null_mut(),
            error_recv: erx,
            module: std::marker::PhantomData
        }));

        let vm_config = Box::into_raw(Box::new(UserData {
            error_channel: etx,
            printer: Box::new(p),
            vm: Rc::downgrade(&wvm),
            loader: Box::new(l),
            library,
        }));

        // Configure the Wren side of things
        let mut config = unsafe {
            let mut uconfig = mem::MaybeUninit::<WrenConfiguration>::zeroed();
            wren_sys::wrenInitConfiguration(uconfig.as_mut_ptr());
            let mut config = uconfig.assume_init();
            // Stuff the callbacks into user data
            config.errorFn = Some(wren_error);
            config.writeFn = Some(wren_print);
            config.reallocateFn = Some(wren_realloc);
            config.bindForeignMethodFn = Some(wren_bind_foreign_method);
            config.bindForeignClassFn = Some(wren_bind_foreign_class);
            config.loadModuleFn = Some(wren_load_module);
            config.userData = vm_config as *mut ffi::c_void;
            config
        };

        let vm = unsafe { wren_sys::wrenNewVM(&mut config) };
        wvm.borrow_mut().vm = vm;
        wvm
    }

    pub fn call(&self, handle: &Handle) -> Result<(), VMError> {
        // TODO Do we need to check if this handle came from this VM, or does Wren do that for us?
        match unsafe { wren_sys::wrenCall(self.vm, handle.handle) } {
            wren_sys::WrenInterpretResult_WREN_RESULT_SUCCESS => Ok(()),
            wren_sys::WrenInterpretResult_WREN_RESULT_COMPILE_ERROR => unreachable!("wrenCall doesn't compile anything"),
            wren_sys::WrenInterpretResult_WREN_RESULT_RUNTIME_ERROR => {
                let mut error = "".to_string();
                let mut frames = vec![];
                while let Ok(err) = self.error_recv.try_recv() {
                    match err {
                        WrenError::Runtime(msg) => {error = msg; },
                        WrenError::StackTrace(module, line, msg) => {frames.push(VMStackFrameError {
                            module, line, function: msg
                        }); },
                        _ => unreachable!()
                    }
                }
                Err(VMError::Runtime{
                    error,
                    frames
                })
            },
            _ => unreachable!()
        }
    }

    pub fn interpret<M: AsRef<str>, C: AsRef<str>>(&self, module: M, code: C) -> Result<(), VMError> {
        let module = ffi::CString::new(module.as_ref()).expect("module name conversion failed");
        let code = ffi::CString::new(code.as_ref()).expect("code conversion failed");
        match unsafe { wren_sys::wrenInterpret(self.vm, module.as_ptr() as *const i8, code.as_ptr() as *const i8) } {
            wren_sys::WrenInterpretResult_WREN_RESULT_SUCCESS => Ok(()),
            wren_sys::WrenInterpretResult_WREN_RESULT_COMPILE_ERROR => match self.error_recv.try_recv() {
                Ok(WrenError::Compile(module, line, msg)) => {
                    Err(VMError::Compile { module, line, error: msg })
                }
                _ => unreachable!()
            },
            wren_sys::WrenInterpretResult_WREN_RESULT_RUNTIME_ERROR => {
                let mut error = "".to_string();
                let mut frames = vec![];
                while let Ok(err) = self.error_recv.try_recv() {
                    match err {
                        WrenError::Runtime(msg) => {error = msg; },
                        WrenError::StackTrace(module, line, msg) => {frames.push(VMStackFrameError {
                            module, line, function: msg
                        }); },
                        _ => unreachable!()
                    }
                }
                Err(VMError::Runtime{
                    error,
                    frames
                })
            },
            _ => unreachable!()
        }
    }

    // manual GC trigger
    pub fn collect_garbage(&self) {
        unsafe {
            wren_sys::wrenCollectGarbage(self.vm)
        }
    }

    // Slot and Handle API
    pub fn ensure_slots(&self, count: usize) {
        unsafe {
            wren_sys::wrenEnsureSlots(self.vm, count as raw::c_int)
        }
    }

    pub fn get_slot_count(&self) -> usize {
        unsafe {
            wren_sys::wrenGetSlotCount(self.vm) as usize
        }
    }

    // TODO Change slots from i32 to usize
    // TODO Make a value-agnostic slot API too.

    pub fn set_slot_bool(&self, slot: i32, val: bool) {
        unsafe {
            wren_sys::wrenSetSlotBool(self.vm, slot as raw::c_int, val)
        }
    }

    pub fn set_slot_double(&self, slot: i32, val: f64) {
        unsafe {
            wren_sys::wrenSetSlotDouble(self.vm, slot as raw::c_int, val)
        }
    }

    pub fn set_slot_null(&self, slot: i32) {
        unsafe {
            wren_sys::wrenSetSlotNull(self.vm, slot as raw::c_int)
        }
    }

    pub fn set_slot_bytes(&self, slot: i32, bytes: &[u8]) {
        unsafe {
            wren_sys::wrenSetSlotBytes(self.vm, slot as raw::c_int, bytes as *const _ as *const raw::c_char, bytes.len() as wren_sys::size_t);
        }
    }

    pub fn set_slot_string<S: AsRef<str>>(&self, slot: i32, string: S) {
        let string = string.as_ref();
        unsafe {
            wren_sys::wrenSetSlotBytes(self.vm, slot as raw::c_int, string.as_ptr() as *const _, string.len() as wren_sys::size_t);
        }
    }

    pub fn get_slot_bool(&self, slot: i32) -> bool {
        unsafe {
            wren_sys::wrenGetSlotBool(self.vm, slot as raw::c_int)
        }
    }

    pub fn get_slot_double(&self, slot: i32) -> f64 {
        unsafe {
            wren_sys::wrenGetSlotDouble(self.vm, slot as raw::c_int)
        }
    }

    pub fn get_slot_bytes(&self, slot: i32) -> Vec<u8> {
        let mut length = 0 as raw::c_int;
        let ptr = unsafe {
            wren_sys::wrenGetSlotBytes(self.vm, slot as raw::c_int, &mut length as *mut _)
        };
        let mut bytes = vec![];

        // Do some pointer maths to get the vector. Hurrah!
        for offset in 0..length {
            unsafe {
                bytes.push(*ptr.offset(offset as isize) as u8)
            }
        }

        bytes
    }

    pub fn get_slot_string(&self, slot: i32) -> String {
        let ptr = unsafe {
            wren_sys::wrenGetSlotString(self.vm, slot as raw::c_int)
        };

        let cstr = unsafe{ ffi::CStr::from_ptr(ptr) };

        cstr.to_string_lossy().to_string()
    }

    pub fn get_slot_type(&self, slot: i32) -> SlotType {
        match unsafe { wren_sys::wrenGetSlotType(self.vm, slot as raw::c_int) } {
            wren_sys::WrenType_WREN_TYPE_NUM => SlotType::Num,
            wren_sys::WrenType_WREN_TYPE_BOOL => SlotType::Bool,
            wren_sys::WrenType_WREN_TYPE_LIST => SlotType::List,
            wren_sys::WrenType_WREN_TYPE_NULL => SlotType::Null,
            wren_sys::WrenType_WREN_TYPE_STRING => SlotType::String,
            wren_sys::WrenType_WREN_TYPE_FOREIGN => SlotType::Foreign,
            wren_sys::WrenType_WREN_TYPE_UNKNOWN => SlotType::Unknown,
            _ => unreachable!()
        }
    }

    pub fn get_variable<M: AsRef<str>, N: AsRef<str>>(&self, module: M, name: N, slot: i32) {
        let module = ffi::CString::new(module.as_ref()).expect("module name conversion failed");
        let name = ffi::CString::new(name.as_ref()).expect("variable name conversion failed");
        unsafe {
            wren_sys::wrenGetVariable(self.vm, module.as_ptr(), name.as_ptr(), slot as raw::c_int)
        }
    }

    pub fn set_slot_new_list(&self, slot: i32) {
        unsafe {
            wren_sys::wrenSetSlotNewList(self.vm, slot as raw::c_int)
        }
    }

    pub fn insert_in_list(&self, list_slot: i32, index: i32, element_slot: i32) {
        unsafe {
            wren_sys::wrenInsertInList(
                self.vm, 
                list_slot as raw::c_int,
                index as raw::c_int,
                element_slot as raw::c_int
            )
        }
    }

    pub fn get_slot_handle(&self, slot: i32) -> Handle {
        Handle {
            handle: unsafe {
                wren_sys::wrenGetSlotHandle(self.vm, slot as raw::c_int)
            },
            vm: self
        }
    }

    pub fn set_slot_handle(&self, slot: i32, handle: &Handle) {
        unsafe {
            wren_sys::wrenSetSlotHandle(self.vm, slot as raw::c_int, handle.handle)
        }
    }

    pub fn get_slot_foreign<T: 'static + ClassObject>(&self, slot: i32) -> Option<&T> {
        self.get_slot_foreign_mut(slot).map(|mr| &*mr)
    }

    pub fn get_slot_foreign_mut<T: 'static + ClassObject>(&self, slot: i32) -> Option<&mut T> {
        unsafe {
            let ptr = wren_sys::wrenGetSlotForeign(self.vm, slot as raw::c_int);
            if ptr != std::ptr::null_mut() {
                let fo = &mut *(ptr as *mut ForeignObject<T>);
                if fo.type_id == any::TypeId::of::<T>() {
                    // Safe to downcast
                    fo.object.as_mut()
                } else {
                    // Incorrect type, unsafe to downcast
                    None
                }
            } else {
                None
            }
        }
    }

    /// Looks up the specifed [module] for the specified [class]
    /// If it's type matches with type T, will create a new instance in [slot]
    /// WARNING: This *will* overwrite slot 0, so be careful.
    pub fn set_slot_new_foreign<M: AsRef<str>, C: AsRef<str>, T: 'static + ClassObject>(&self, module: M, class: C, object: T, slot: i32) -> Option<&mut T> {
        let conf = unsafe { &mut *(wren_sys::wrenGetUserData(self.vm) as *mut UserData) };

        self.ensure_slots((slot + 1) as usize);
        // Even if slot == 0, we can just load the class into slot 0, then use wrenSetSlotNewForeign to "create" a new object
        match conf.library.and_then(|lib| lib.get_foreign_class(module.as_ref(), class.as_ref())) {
            None => None, // Couldn't find the corresponding class
            Some(runtime_class) => {
                if runtime_class.type_id == any::TypeId::of::<T>() {
                    // The Wren foreign class corresponds with this real object.
                    // We can coerce it and treat this object as that class, even if not instantiated by Wren.

                    // Create the new ForeignObject
                    let new_obj = Box::new(ForeignObject {
                        object: Box::into_raw(Box::new(object)),
                        type_id: any::TypeId::of::<T>(),
                    });

                    // Load the Wren class object into slot 0.
                    self.get_variable(module, class, 0);

                    unsafe {
                        // Create the Wren foreign pointer
                        let wptr = wren_sys::wrenSetSlotNewForeign(self.vm, slot as raw::c_int, 0, mem::size_of::<ForeignObject<T>>() as wren_sys::size_t);
                        
                        // Move the ForeignObject into the pointer
                        std::ptr::copy_nonoverlapping(Box::leak(new_obj), wptr as *mut _, 1);

                        // Reinterpret the pointer as an object if we were successful
                        (wptr as *mut T).as_mut()
                    }
                } else {
                    // The classes do not match. Avoid.
                    None
                }
            }
        }
    }

    pub fn make_call_handle<S: AsRef<str>>(&self, signature: S) -> Handle {
        let signature = ffi::CString::new(signature.as_ref()).expect("signature conversion failed");
        Handle {
            handle: unsafe {
                wren_sys::wrenMakeCallHandle(self.vm, signature.as_ptr())
            },
            vm: self
        }
    }

    pub fn abort_fiber(&self, slot: i32) {
        unsafe {
            wren_sys::wrenAbortFiber(self.vm, slot as raw::c_int)
        }
    }
}

impl<'a> Drop for VM<'a> {
    fn drop(&mut self) {
        unsafe {
            let conf = wren_sys::wrenGetUserData(self.vm);
            Box::from_raw(conf); // Drop the userdata
            wren_sys::wrenFreeVM(self.vm);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Executor, create_class_objects};

    struct Point {
        x: f64,
    }

    impl Point {
        fn x(&self, vm: &super::VM) {
            vm.ensure_slots(1);
            vm.set_slot_double(0, self.x);
        }

        fn set_x(&mut self, vm: &super::VM) {
            vm.ensure_slots(2);
            if vm.get_slot_type(1) != super::SlotType::Num { panic!("x must be a number"); }
            let new_x = vm.get_slot_double(1);
            self.x = new_x;
        }
    }

    impl super::Class for Point {
        fn initialize(vm: &super::VM) -> Point {
            vm.ensure_slots(2);
            if vm.get_slot_type(1) != super::SlotType::Num { panic!("constructor must be (<num>)")};
            let x = vm.get_slot_double(1);
            Point {
                x,
            }
        }
    }

    struct Math;

    impl super::Class for Math {
        fn initialize(_: &super::VM) -> Math { Math }
    }

    impl Math {
        fn add5(vm: &super::VM) {
            vm.ensure_slots(2);
            assert_eq!(vm.get_slot_type(1), super::SlotType::Num);
            let i = vm.get_slot_double(1);
            vm.set_slot_double(0, i + 5.0);
        }

        fn pointy(vm: &super::VM) {
            vm.ensure_slots(2);
            let send = vm.set_slot_new_foreign("main", "RawPoint", Point {
                x: 345.7
            }, 0);
            if send.is_none() {
                panic!("Could not send RawPoint object");
            }
        }
    }

    create_class_objects! {
        class("RawPoint") crate::tests::Point => point {
            instance("x()") x,
            instance("set_x(_)") set_x
        }

        class("Math") crate::tests::Math => math {
            static("add5(_)") add5,
            static("pointy()") pointy
        }

        module => main
    }

    #[test]
    fn init_vm() {
        let _ = super::VM::new_terminal(None);
    }

    #[test]
    fn test_small_wren_program() {
        let vm = super::VM::new_terminal(None);
        vm.execute(|vm| {
            let interp = vm.interpret("main", "System.print(\"I am running in a VM!\")");
            println!("{:?}", interp);
            assert!(interp.is_ok());
        });
    }

    #[test]
    fn test_small_wren_program_call() {
        let vm = super::VM::new_terminal(None);

        vm.execute(|vm| {
            let source = vm.interpret("main", r"
            class GameEngine {
                static update(elapsedTime) {
                    System.print(elapsedTime)
                    return 16.45
                }
            }
            ");
            assert!(source.is_ok());
        });

        vm.execute(|vm| {
            vm.ensure_slots(2);
            vm.get_variable("main", "GameEngine", 0);
            let _ = vm.get_slot_handle(0);
            vm.set_slot_double(1, 32.2);
            let update_handle = vm.make_call_handle("update(_)");
            let interp = vm.call(&update_handle);
            assert!(interp.is_ok());
            assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
            assert_eq!(vm.get_slot_double(0), 16.45);
        });
    }

    #[test]
    fn test_external_module() {
        let mut lib = super::ModuleLibrary::new();
        main::publish_module(&mut lib);
        let vm = super::VM::new_terminal(Some(&lib));
        vm.execute(|vm| {
            let source = vm.interpret("main", "
            class Math {
                foreign static add5(a)
            }

            foreign class RawPoint {
                construct new(x) {}

                foreign x()
                foreign set_x(val)
            }

            class Point {
                construct new(x) {
                    _rp = RawPoint.new(x)
                }

                x { _rp.x() }
                x=(val) { _rp.set_x(val) }
            }

            class GameEngine {
                static update(elapsedTime) {
                    System.print(elapsedTime)
                    var p = Point.new(3)
                    System.print(p.x)
                    p.x = 10
                    System.print(p.x)
                    return Math.add5(16.45)
                }
            }
            ");
            println!("{:?}", source);
            assert!(source.is_ok());
        });

        vm.execute(|vm| {
            vm.ensure_slots(2);
            vm.get_variable("main", "GameEngine", 0);
            let _ = vm.get_slot_handle(0);
            vm.set_slot_double(1, 32.2);
            let update_handle = vm.make_call_handle("update(_)");
            let interp = vm.call(&update_handle);
            if let Err(e) = interp.clone() {
                eprintln!("{}", e);
            }
            assert!(interp.is_ok());
            assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
            assert_eq!(vm.get_slot_double(0), 21.45);
        });
    }

    #[test]
    fn test_script_module() {
        struct TestLoader;

        impl super::ModuleScriptLoader for TestLoader {
            fn load_script(&mut self, name: String) -> Option<String> {
                if name == "math" {
                    Some("
                    class Math {
                        static add5(val) {
                            return val + 5
                        }
                    }
                    ".into())
                } else {
                    None
                }
            }
        }

        let vm = super::VM::new(super::PrintlnPrinter, TestLoader, None);
        vm.execute(|vm| {
            let source = vm.interpret("main", "
            import \"math\" for Math

            class GameEngine {
                static update(elapsedTime) {
                    System.print(elapsedTime)
                    return Math.add5(16.45)
                }
            }
            ");
            assert!(source.is_ok());
        });

        vm.execute(|vm| {
            vm.ensure_slots(2);
            vm.get_variable("main", "GameEngine", 0);
            let _ = vm.get_slot_handle(0);
            vm.set_slot_double(1, 32.2);
            let update_handle = vm.make_call_handle("update(_)");
            let interp = vm.call(&update_handle);
            assert!(interp.is_ok());
            assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
            assert_eq!(vm.get_slot_double(0), 21.45);
        });
    }

    #[test]
    fn foreign_instance() {
        let mut lib = super::ModuleLibrary::new();
        main::publish_module(&mut lib);
        let vm = super::VM::new_terminal(Some(&lib));
        vm.execute(|vm| {
            let source = vm.interpret("main", "
            class Math {
                foreign static add5(a)
                foreign static pointy()
            }

            foreign class RawPoint {
                construct new(x) {}

                foreign x()
                foreign set_x(val)
            }

            class GameEngine {
                static update(elapsedTime) {
                    System.print(elapsedTime)
                    var p = Math.pointy()
                    System.print(p.x())
                    return Math.add5(16.45)
                }
            }
            ");
            println!("{:?}", source);
            assert!(source.is_ok());
        });

        vm.execute(|vm| {
            vm.ensure_slots(2);
            vm.get_variable("main", "GameEngine", 0);
            let _ = vm.get_slot_handle(0);
            vm.set_slot_double(1, 32.2);
            let update_handle = vm.make_call_handle("update(_)");
            let interp = vm.call(&update_handle);
            if let Err(e) = interp.clone() {
                eprintln!("{}", e);
            }
            assert!(interp.is_ok());
            assert_eq!(vm.get_slot_type(0), super::SlotType::Num);
            assert_eq!(vm.get_slot_double(0), 21.45);
        });
    }
}
