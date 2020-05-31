# Ruwren: Wren bindings for Rust [![Crates.io](https://img.shields.io/crates/v/ruwren)](https://crates.io/crates/ruwren) [![badge](https://docs.rs/ruwren/badge.svg)](https://docs.rs/ruwren/)

Here is an attempt at making some Rust Wren bindings in a more Rust style.
It act pretty low-level still (you do have to deal with the slot / foreign API), but should be 

- Slightly more typesafe than plain C
- Shouldn't get in the way of quick execution
- Should be relatively simple to get started