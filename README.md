ocaml-vdom: Elm architecture and (V)DOM for OCaml
=================================================

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
git clone https://github.com/LexiFi/ocaml-vdom.git
cd vdom
make all
make doc
make demo   # Optional, to build demo.js (loaded from demo.html)
make install
````


DOM bindings
------------

[`Js_browser`](js_browser.mli) expose (partial) OCaml bindings of the browser's DOM and
other common client-side Javascript API.

It is implemented with
[gen_js_api](https://github.com/LexiFi/gen_js_api), making it
realistic to write code portable between js_of_ocaml and Bucklescript
in the future.


VDOM
----

The [Elm architecture](https://guide.elm-lang.org/architecture/) is a
functional way to describe UI applications.  In this architecture, the
current state of the UI is represented with a single data type and a
"view" function projects this state to a concrete rendering.  In our
case, this rendering is done to a tree-like abstraction of the browser
DOM, called a VDOM (Virtual DOM).  This VDOM can itself be rendered to
a concrete DOM.  Whenever the state changes, the view function produces
a new VDOM tree, which is then diffed with the previous one to update
the concrete DOM accordingly.  The VDOM also specifies how DOM events
are wrapped into "messages" that are processed by an "update" function
to modify the current state.  This function can also spawn "commands"
(such as AJAX calls) whose outcome is also notified by messages.


The implementation of this architecture relies on two modules:

  - [`Vdom`](vdom.mli) : definition of the VDOM tree and of "virtual
    applications".  This is a "pure" module, which does not depend on
    any Javascript bindings (it could be executed on the server-side,
    e.g. for automated testing).

  - [`Vdom_blit`](vdom_blit.mli) : rendering of virtual applications into the actual
    DOM.  This modules implements the initial "blit" operation
    (rendering a VDOM tree to the DOM) and the "diff/synchronization"
    algorithm.  It also manages the state of a running application.
    `Vdom_blit` is implemented on top of `Js_browser`.
