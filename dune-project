(lang dune 2.0)

(generate_opam_files true)

(source
 (github c-cube/thrifty))

(authors "c-cube")

(maintainers "c-cube")

(license MIT)

(package
 (name thrifty)
 (synopsis
   "Runtime library for Thrifty, a reimplementation of thrift for OCaml")
 (depends
  (ocaml
   (>= 4.08))
  dune)
 (tags
  (topics thrift rpc serialization)))

(package
 (name thrifty_compiler)
 (synopsis "IDL compiler for thrifty")
 (depends
  (ocaml
   (>= 4.08))
  (thrifty
   (= :version))
  (containers
   (>= 3.6))
  dune)
 (tags
  (topics thrift rpc serialization)))
