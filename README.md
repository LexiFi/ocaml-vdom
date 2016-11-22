vdom: Elm architecture and (V)DOM for OCaml
===========================================

Overview
--------

This package contains:

  - OCaml bindings to DOM and other client-side Javascript APIs
    (using [gen_js_api](https://github.com/LexiFi/gen_js_api)).

  - An implementation of the [Elm architecture](https://guide.elm-lang.org/architecture/), where the
    UI is specified as a functional "view" on the current state.



Dependencies
------------

  - OCaml

  - js_of_ocaml

  - findlib

  - gen_js_api



Installation (with OPAM)
------------------------


Not yet available.


Manual installation
-------------------

````
git clone https://github.com/LexiFi/vdom.git
cd vdom
make all
make doc
make demo   # Optional, to build demo.js (loaded from demo.html)
make install
````
