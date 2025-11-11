set shell := ["nu", "-c"]
set dotenv-load

build *flags:
    ^cargo build {{flags}}

run *flags:
    ^cargo run {{flags}}

all-examples:
    ls examples | \
        each {|ex| $ex.name | parse -r 'examples/(?P<ename>.*)\.rs' | get ename} | \
        flatten | \
        each {|exname| ^cargo run --example $exname}

example name *flags:
    with-env { RUST_BACKTRACE: 1 } { ^cargo run --example {{name}} {{flags}} }

example-rel name *flags:
    with-env { RUST_BACKTRACE: 1 } { ^cargo run --release --example {{name}} {{flags}} }

expand example:
    ^cargo expand --example {{example}}

wasm-example name *wasmtime_flags:
    with-env { \
        CARGO_TARGET_WASM32_WASI_LINKER: $"($env.WASI_SDK_PATH)/bin/wasm-ld" \
        CC_wasm32-wasi: $"($env.WASI_SDK_PATH)/bin/clang" \
        AR_wasm32-wasi: $"($env.WASI_SDK_PATH)/bin/llvm-ar" \
        RANLIB_wasm32-wasi: $"($env.WASI_SDK_PATH)/bin/llvm-ranlib" \
        CFLAGS_wasm32-wasi: "-D_WASI_EMULATED_PROCESS_CLOCKS" \
        CARGO_TARGET_WASM32_WASI_RUSTFLAGS: $"-C link-args=-L($env.WASI_SDK_PATH)/share/wasi-sysroot/lib/wasm32-wasi" \
        BINDGEN_EXTRA_CLANG_ARGS_wasm32-wasi: $"--sysroot=($env.WASI_SDK_PATH)/share/wasi-sysroot -fvisibility=default" \
        CARGO_TARGET_WASM32_WASI_RUNNER: "wasmtime -Scommon {{wasmtime_flags}}" \
    } { \
        ^cargo run --example {{name}} --target wasm32-wasi \
    }
