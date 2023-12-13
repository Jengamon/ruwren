let includes: list<string> = (^zig libc -target wasm32-wasi -includes | lines)
with-env {
    RUST_BACKTRACE: 1
    RUSTC_LINKER: "zig cc"
    CC: "zig cc"
    AR: "zig ar"
    RANLIB: "zig ranlib"
    TARGET: "wasm32-wasi"
    TARGET_CFLAGS: "-target wasm32-wasi -std=c11"
    BINDGEN_EXTRA_CLANG_ARGS: ($includes | reverse | each {|inc| $"-isystem($inc)"} | str join " " | $"-include stddef.h ($in)") # | $"-lc")
    CFLAGS: "-D_WASI_EMULATED_PROCESS_CLOCKS"
}   { ^cargo run --target wasm32-wasi -p ruwren_web_example }
