example-wasi:
    RUST_BACKTRACE=1 RUSTC_LINKER="zig cc" CC="zig cc" AR="zig ar" TARGET="wasm32-wasi" CFLAGS="--target=wasm32-wasi -D_WASI_EMULATED_PROCESS_CLOCKS" cargo run --target wasm32-wasi -p ruwren_web_example