(lang dune 3.0)

(name vdom)

(generate_opam_files true)

(version 0.3)

(maintainers "Alain Frisch <alain.frisch@lexifi.com>")

(authors
 "Alain Frisch <alain.frisch@lexifi.com>"
 "Marc Lasson <marc.lasson@lexifi.com>"
 "Aurélien Saue <aurelien.saue@lexifi.com>")

(source
 (github LexiFi/ocaml-vdom))

(package
 (name vdom)
 (synopsis "DOM and VDOM for OCaml")
 (description
  "This package contains:
- OCaml bindings to DOM and other client-side Javascript APIs (using gen_js_api).
- An implementation of the Elm architecture, where the UI is specified as a functional \"view\" on the current state.")
 (license MIT)
 (depends
  (ocaml (>= 4.10))
  (gen_js_api (>= 1.0.7))
  ojs
  js_of_ocaml-compiler)
 (conflicts (ocaml-vdom (<> transition))))
