//! We need to expose the Wren API in a Rust-y way
use wren_sys::{WrenVM, WrenHandle, WrenConfiguration, WrenErrorType};
use std::sync::mpsc::{channel, Sender, Receiver};

use std::mem;
use std::ffi;
use std::os::raw;

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
    conf.print_channel.send(message_str.to_string_lossy().to_string()).unwrap();
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
                    writeln!(fmt, "\tin {}:{}: {}", frame.module, frame.line, frame.function)?;
                }
                Ok(())
            },
        }
    }
}

impl std::error::Error for VMError {}

pub struct Handle<'a> {
    handle: *mut WrenHandle,
    vm: &'a VM
}

impl<'a> Drop for Handle<'a> {
    fn drop(&mut self) {
        unsafe {
            wren_sys::wrenReleaseHandle(self.vm.vm, self.handle);
        }
    }
}

pub struct VM {
    vm: *mut WrenVM,
    error_recv: Receiver<WrenError>,
    print_recv: Receiver<String>,
}

pub struct UserData {
    pub error_channel: Sender<WrenError>,
    pub print_channel: Sender<String>,
}

impl VM {
    pub fn new() -> VM {
        let (etx, erx) = channel();
        let (ptx, prx) = channel();

        // Have an uninitialized VM...
        let mut wvm = VM {
            vm: std::ptr::null_mut(),
            error_recv: erx,
            print_recv: prx,
        };

        let vm_config = Box::into_raw(Box::new(UserData {
            error_channel: etx,
            print_channel: ptx
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
            config.userData = vm_config as *mut ffi::c_void;
            config
        };

        let vm = unsafe { wren_sys::wrenNewVM(&mut config) };
        wvm.vm = vm;
        wvm
    }

    pub fn printed_strings(&self) -> Vec<String> {
        self.print_recv.try_iter().collect()
    }

    pub fn call<M: AsRef<str>, C: AsRef<str>>(&self, handle: &Handle) -> Result<(), VMError> {
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
}

impl Drop for VM {
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
    #[test]
    fn init_vm() {
        let vm = super::VM::new();
    }

    #[test]
    fn test_small_wren_program() {
        let vm = super::VM::new();
        let interp = vm.interpret("main", "System.print(\"I am running in a VM!\")");
        println!("{:?}", interp);
        assert!(interp.is_ok());
        assert_eq!(vm.printed_strings(), vec!["I am running in a VM!", "\n"]);
    }
}
