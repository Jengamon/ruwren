use alloc::string::{String, ToString};
use core::ffi;

use crate::{UserData, WrenError};
use wren_sys::{WrenErrorType, WrenForeignClassMethods, WrenLoadModuleResult, WrenVM};

// Force Wren to use Rust's allocator to allocate memory
// Done because sometimes Wren forces us to allocate memory and give *it* ownership
// Rust might not use the standard allocator, so we move Wren to use *our* allocator
pub unsafe extern "C" fn wren_realloc(
    memory: *mut ffi::c_void, new_size: usize, _user_data: *mut ffi::c_void,
) -> *mut ffi::c_void {
    unsafe {
        if memory.is_null() {
            // If memory == NULL
            if new_size == 0 {
                core::ptr::null_mut()
            } else {
                // allocate new memory
                alloc::alloc::alloc_zeroed(
                    alloc::alloc::Layout::from_size_align(new_size, 8).unwrap(),
                ) as *mut _
            }
        } else {
            // Memory is an actual pointer to a location.
            if new_size == 0 {
                alloc::alloc::dealloc(
                    memory as *mut _,
                    alloc::alloc::Layout::from_size_align(0, 8).unwrap(),
                );
                core::ptr::null_mut()
            } else {
                alloc::alloc::realloc(
                    memory as *mut _,
                    alloc::alloc::Layout::from_size_align(new_size, 8).unwrap(),
                    new_size,
                ) as *mut _
            }
        }
    }
}

pub extern "C" fn wren_error(
    vm: *mut WrenVM, typ: WrenErrorType, module: *const ffi::c_char, line: ffi::c_int,
    message: *const ffi::c_char,
) {
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    match typ {
        wren_sys::WrenErrorType_WREN_ERROR_COMPILE => {
            let module_str = unsafe { ffi::CStr::from_ptr(module) };
            let message_str = unsafe { ffi::CStr::from_ptr(message) };
            conf.error_channel
                .send(WrenError::Compile(
                    module_str.to_string_lossy().to_string(),
                    line,
                    message_str.to_string_lossy().to_string(),
                ))
                .unwrap();
        }
        wren_sys::WrenErrorType_WREN_ERROR_RUNTIME => {
            let message_str = unsafe { ffi::CStr::from_ptr(message) };
            conf.error_channel
                .send(WrenError::Runtime(
                    message_str.to_string_lossy().to_string(),
                ))
                .unwrap();
        }
        wren_sys::WrenErrorType_WREN_ERROR_STACK_TRACE => {
            let module_str = unsafe { ffi::CStr::from_ptr(module) };
            let message_str = unsafe { ffi::CStr::from_ptr(message) };
            conf.error_channel
                .send(WrenError::StackTrace(
                    module_str.to_string_lossy().to_string(),
                    line,
                    message_str.to_string_lossy().to_string(),
                ))
                .unwrap();
        }
        _ => unreachable!(),
    }
}

pub extern "C" fn wren_print(vm: *mut WrenVM, message: *const ffi::c_char) {
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let message_str = unsafe { ffi::CStr::from_ptr(message) };
    conf.printer
        .print(message_str.to_string_lossy().to_string());
}

pub extern "C" fn wren_bind_foreign_method(
    vm: *mut WrenVM, mdl: *const ffi::c_char, class: *const ffi::c_char, is_static: bool,
    sgn: *const ffi::c_char,
) -> Option<unsafe extern "C" fn(*mut WrenVM)> {
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let module = unsafe { ffi::CStr::from_ptr(mdl) };
    let class = unsafe { ffi::CStr::from_ptr(class) };
    let signature = unsafe { ffi::CStr::from_ptr(sgn) };

    if let Some(ref library) = conf.library {
        if let Some(rc) =
            library.get_foreign_class(module.to_string_lossy(), class.to_string_lossy())
        {
            rc.methods
                .function_pointers
                .iter()
                .find(|mp| {
                    mp.signature.as_wren_string() == signature.to_string_lossy()
                        && mp.is_static == is_static
                })
                .map(|mp| mp.pointer)
        } else {
            None
        }
    } else {
        None
    }
}

pub extern "C" fn wren_bind_foreign_class(
    vm: *mut WrenVM, mdl: *const ffi::c_char, class: *const ffi::c_char,
) -> WrenForeignClassMethods {
    let mut fcm = WrenForeignClassMethods {
        allocate: None,
        finalize: None,
    };

    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let module = unsafe { ffi::CStr::from_ptr(mdl) };
    let class = unsafe { ffi::CStr::from_ptr(class) };

    if let Some(ref library) = conf.library {
        let rc = library.get_foreign_class(module.to_string_lossy(), class.to_string_lossy());
        if let Some(rc) = rc {
            fcm.allocate = Some(rc.construct);
            fcm.finalize = Some(rc.destruct);
        }
    }
    fcm
}

pub extern "C" fn wren_load_module(
    vm: *mut WrenVM, name: *const ffi::c_char,
) -> WrenLoadModuleResult {
    // The whoooole reason we wrote wren_realloc - to force Wren into Rust's allocation space
    let conf = unsafe { &mut *(wren_sys::wrenGetUserData(vm) as *mut UserData) };
    let module_name = unsafe { ffi::CStr::from_ptr(name) };
    let source = match conf
        .loader
        .load_script(module_name.to_string_lossy().to_string())
    {
        Some(string) => alloc::ffi::CString::new(string)
            .unwrap_or_else(|_| {
                panic!(
                    "Failed to convert source to C string for {}",
                    module_name.to_string_lossy()
                )
            })
            .into_raw(),
        None => core::ptr::null_mut(),
    };

    WrenLoadModuleResult {
        source,
        onComplete: None,
        userData: core::ptr::null_mut(),
    }
}

pub extern "C" fn wren_canonicalize(
    _: *mut WrenVM, importer: *const ffi::c_char, name: *const ffi::c_char,
) -> *const ffi::c_char {
    let _importer = unsafe { ffi::CStr::from_ptr(importer) };
    let _name = unsafe { ffi::CStr::from_ptr(name) };
    let _importer = _importer.to_string_lossy();
    let _name = _name.to_string_lossy();

    if let Some('@') = _name.chars().next() {
        let real_name: String = _name.chars().skip(1).collect();
        alloc::ffi::CString::new(alloc::format!("{}/{}", _importer, real_name))
            .unwrap_or_else(|_| {
                panic!(
                    "Failed to convert name {}/{} to C string",
                    _importer, real_name
                )
            })
            .into_raw() as *const _
    } else {
        name
    }
}
