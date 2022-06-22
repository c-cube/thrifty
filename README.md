
# Thrifty [![build](https://github.com/c-cube/thrifty/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/smol_thrift/actions/workflows/main.yml)

A thrift reimplementation in OCaml.

The main goals are:
- efficient implementation for the common features (compact protocol)
- readable, modern OCaml
- relatively simple implementation

## License

MIT license


## Features

### Protocols

- [x] compact protocol
- [x] binary protocol

### Transports

- [x] file transport
- [x] string transport
- [ ] TCP server

### IDL Compiler

- [x] parser
- [x] code generation for types
- [x] code generation for printers
- [x] code generation for codecs
- [ ] (wip) code generation for services
