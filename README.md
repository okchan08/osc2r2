# osc2r2 - Osc2r2 is an OpenSCENARIO 2.0 Rust runner

## Overview

Osc2r2 is an open source OpenSCENARIO player written in Rust.

This repository contains the following features/libraries

- ASAM OpenDRIVE viewer. A library that reads ASAM OpenDRIVE standard format and visualizes in 3D viewer.
- ASAM OpenSCENARIO 2.0 player (still under implementation)

## Usage

The binaries can be executed via standard [Cargo](https://doc.rust-lang.org/cargo/commands/cargo-run.html#target-selection) command.

All binaries are located under the binary directory.

### OpenDRIVE viewer.

```sh
cargo run --bin odrviewer
```

## Acknowledgements

The OpenDRIVE viewer references [libOpenDRIVE](https://github.com/pageldev/libOpenDRIVE) by pageldev.
