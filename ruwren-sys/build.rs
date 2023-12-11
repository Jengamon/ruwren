use std::env;
use std::path::PathBuf;

// TODO Make the optional modules....optional

fn main() {
    // Tell cargo to invalidate the built crate whenever the wrapper changes
    // println!("cargo:rerun-if-changed=wrapper.h");

    cc::Build::new()
        .files(
            glob::glob("src/*.c")
                .expect("Failed to glob c files")
                .filter_map(|x| x.ok()),
        )
        .files(
            glob::glob("optional/*.c")
                .expect("Failed to glob c files")
                .filter_map(|x| x.ok()),
        )
        .include("include")
        .include("optional")
        .include("src")
        .compile("wren");

    let bindings = bindgen::Builder::default()
        .detect_include_paths(true)
        .header("wrapper.h")
        .allowlist_var("WREN.*")
        .allowlist_type("Wren.*")
        .allowlist_function("wren.*")
        .clang_arg("-Isrc")
        .clang_arg("-Iinclude")
        .clang_arg("-Ioptional")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::default()))
        .generate()
        .expect("Unable to generate bindings.");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
