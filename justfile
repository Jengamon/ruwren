example name:
    RUST_BACKTRACE=1 cargo run --example {{name}}

example-rel name:
    RUST_BACKTRACE=1 cargo run --release --example {{name}}

expand example:
    cargo expand --example {{example}}
