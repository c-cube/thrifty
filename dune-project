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
  (qcheck-core :with-test)
  (containers :with-test)
  dune)
 (depopts base-unix)
 (tags
  (topics thrift rpc serialization)))

(package
 (name thrifty-lwt)
 (synopsis "Runtime library for Thrifty with Lwt for concurrency")
 (depends
  (ocaml
   (>= 4.08))
  (qcheck-core :with-test)
  lwt
  dune)
 (tags
  (topics thrift rpc serialization lwt)))

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
