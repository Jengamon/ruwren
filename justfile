set shell := ["nu", "-c"]
set dotenv-load

example name:
    with-env { RUST_BACKTRACE: 1 } { ^cargo run --example {{name}} }

example-rel name:
    with-env { RUST_BACKTRACE: 1 } { ^cargo run --release --example {{name}} }

expand example:
    ^cargo expand --example {{example}}

wasm-example name:
    with-env { \
        RUST_BACKTRACE: 1 \
        CARGO_TARGET_WASM32_WASI_LINKER: $"($env.WASI_SDK_PATH)/bin/wasm-ld" \
        CARGO_TARGET_WASM32_WASI_PREVIEW1_THREADS_LINKER: $"($env.WASI_SDK_PATH)/bin/wasm-ld" \
        TARGET: "wasm32-wasi-threads" \
        CC: $"($env.WASI_SDK_PATH)/bin/clang" \
        AR: $"($env.WASI_SDK_PATH)/bin/llvm-ar" \
        RANLIB: $"($env.WASI_SDK_PATH)/bin/llvm-ranlib" \
        CFLAGS_wasm32-wasi: "-D_WASI_EMULATED_PROCESS_CLOCKS" \
        CFLAGS_wasm32-wasi-preview1-threads: "-D_WASI_EMULATED_PROCESS_CLOCKS -pthread" \
        CARGO_TARGET_WASM32_WASI_RUSTFLAGS: $"-C link-args=-L($env.WASI_SDK_PATH)/share/wasi-sysroot/lib/wasm32-wasi" \
        CARGO_TARGET_WASM32_WASI_PREVIEW1_THREADS_RUSTFLAGS: $"-C link-args=-L($env.WASI_SDK_PATH)/share/wasi-sysroot/lib/wasm32-wasi \
        -C link-args=-L($env.WASI_SDK_PATH)/share/wasi-sysroot/lib/wasm32-wasi-threads \
        -C link-args=--import-memory -C link-args=--max-memory=67108864" \
        BINDGEN_EXTRA_CLANG_ARGS: $"--sysroot=($env.WASI_SDK_PATH)/share/wasi-sysroot -fvisibility=default" \
        CARGO_TARGET_WASM32_WASI_RUNNER: "wasmtime" \
        CARGO_TARGET_WASM32_WASI_PREVIEW1_THREADS_RUNNER: "wasmtime -Sthreads -Scommon" \
    } { \
        ^cargo run --example {{name}} --target wasm32-wasi \
    }
