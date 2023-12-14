# the -fvisibility=default flag is to fix a bindgen (clang?) bug on wasm32 where function visiblity is misreported
# you set the WASI_SDK_PATH environment variable to point to a valid WASI SDK install location, and should presto work
with-env {
    RUST_BACKTRACE: 1
    CARGO_TARGET_WASM32_WASI_LINKER: $"($env.WASI_SDK_PATH)/bin/wasm-ld"
    CC: $"($env.WASI_SDK_PATH)/bin/clang"
    AR: $"($env.WASI_SDK_PATH)/bin/llvm-ar"
    RANLIB: $"($env.WASI_SDK_PATH)/bin/llvm-ranlib"
    TARGET: "wasm32-wasi"
    CFLAGS: "-D_WASI_EMULATED_PROCESS_CLOCKS"
    RUSTFLAGS: $"-C link-args=-L($env.WASI_SDK_PATH)/share/wasi-sysroot/lib/wasm32-wasi"
    BINDGEN_EXTRA_CLANG_ARGS_wasm32-wasi: $"--sysroot=($env.WASI_SDK_PATH)/share/wasi-sysroot -fvisibility=default"
    CARGO_TARGET_WASM32_WASI_RUNNER: "wasmtime"
}   {
    ^cargo run --target wasm32-wasi -p ruwren_web_example
}
