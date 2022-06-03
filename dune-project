(lang dune 2.0)

(generate_opam_files true)

(source
 (github c-cube/smol_thrift))

(authors "c-cube")

(maintainers "c-cube")

(license MIT)

(package
 (name smol_thrift)
 (synopsis "Runtime library for Smol_thrift, a reimplementation of thrift")
 (depends
  (ocaml
   (>= 4.08))
  dune)
 (tags
  (topics thrift rpc serialization)))

(package
 (name smol_thrift_compiler)
 (synopsis "IDL compiler for smol_thrift")
 (depends
  (ocaml
   (>= 4.08))
  (smol_thrift
   (= :version))
  (containers
   (>= 3.6))
  dune)
 (tags
  (topics thrift rpc serialization)))
