ocaml-vdom: Elm architecture and (V)DOM for OCaml
=================================================

[![Build Status](https://travis-ci.com/LexiFi/ocaml-vdom.svg?branch=master)](https://travis-ci.com/LexiFi/ocaml-vdom)

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

  - gen_js_api



Installation (with OPAM)
------------------------

````
opam install ocaml-vdom
````


Manual installation
-------------------

````
git clone https://github.com/LexiFi/ocaml-vdom.git
cd ocaml-vdom
make all
make doc
make examples   # Optional (browse index.html files in _build/default/examples to try out)
make install
````


DOM bindings
------------

[`Js_browser`](lib/js_browser.mli) exposes (partial) OCaml bindings of the browser's DOM and
other common client-side Javascript APIs.

It is implemented with
[gen_js_api](https://github.com/LexiFi/gen_js_api), making it
realistic to have it working with Bucklescript in the future.  This
would open the door to writing client-side web applications in OCaml
that could be compiled to Javascript either with js_of_ocaml or
Bucklescript.


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

  - [`Vdom`](lib/vdom.mli) : definition of the VDOM tree and of "virtual
    applications".  This is a "pure" module, which does not depend on
    any Javascript bindings (it could be executed on the server-side,
    e.g. for automated testing).

  - [`Vdom_blit`](lib/vdom_blit.mli) : rendering of virtual applications into the actual
    DOM.  This modules implements the initial "blit" operation
    (rendering a VDOM tree to the DOM) and the "diff/synchronization"
    algorithm.  It also manages the state of a running application.
    `Vdom_blit` is implemented on top of `Js_browser`.



This implementation of VDOM has some specificities:

  - Each node in the VDOM tree has a "key" string field.  By default,
    the key corresponds to the tag name for elements but it can be
    overriden.  The key is used by the synchronization algorithm
    as follows: when synchronizing the old and new children of an
    element, the children are first grouped by key.  Two children with
    different keys are never synchronized, and the sequence of old and
    new children with a given key are synchronized in a pairwise way
    (first old child with key K against first new child with key K;
    etc...), adding or removing extra/missing children if needed.
    Children are also reordered in the DOM, if needed, to match the
    new ordering.

  - Event handlers are not attached on DOM nodes created when a VDOM
    tree is rendered.  Instead, we attach fixed event handlers on the
    root container, and rely on event delegation.  The handler
    corresponding to a given element and responsible for a given kind
    of event is searched directly in the VDOM.  The rationale for this
    design choice is that comparing functional values is not
    well-defined in OCaml, so it would not be clear, when the "old"
    and "new" VDOMs are diffed, if the event handler on the DOM node
    should be refreshed.

  - A "bridge" structure in created in `Vdom_blit` to represent the
    correspondence between VDOM and DOM nodes.  This structure mimics
    the shape of both trees and avoids having to query the concrete
    DOM to navigate in the tree.

  - No data structure is created to represent the "diff" between old
    and new VDOMs.  Instead, the synchronization algorithm detects
    VDOM changes and apply them on the fly to the corresponding DOM
    node.

  - There is some special support for the "value" property.  When this
    property is explicitly bound in the VDOM (typically on an input
    field), the value is forced on the element: whenever the DOM value
    changes, the event is potentially dispatched to an event handler,
    and the new VDOM property is forced on the DOM element.  In
    particular, if the internal state is not updated by the event
    handler, the field becomes in practice read-only.

  - Some special VDOM node attributes are provided to present
    "superficial state changes" that are not reflected in the proper
    functional state (currently: giving focus to an element, or
    ensuring an element is visible by y-scrolling its parent).  These
    attributes produce the corresponding DOM action when they are
    first put on an element (which is not completely well-defined,
    since this depends on the synchronization algorithm).

  - The "view" function is **not** applied synchronously when the
    state ("model") changes.  Instead, a rendering (applying the
    "view" function and updating the actual DOM accordingly) is
    scheduled.  This means that multiple changes can be grouped
    without triggering a redraw.  The current strategy is to delay
    redrawing with [window.requestAnimationFrame](https://developer.mozilla.org/fr/docs/Web/API/Window/requestAnimationFrame), which is supposed to be available (natively,
    or through a polyfill).



Usage
-----

A simple one-module application would look like:

````ocaml
open Vdom

(* Definition of the vdom application *)

type model = .... (* the state of the application *)
let view model =  ...  (* the state->vdom rendering function *)
let init = return ... (* the initial state *)
let update model = function .... (* the state-updating function *)
let my_app = app ~init ~update ~view ()


(* Driver *)

open Js_browser

let run () =
  Vdom_blit.run my_app   (* run the application *)
  |> Vdom_blit.dom    (* get its root DOM container *)
  |> Element.append_child (Document.body document)   (* insert the DOM in the document *)

let () = Window.set_onload window run
````

Compiling this to Javascript:

    ocamlfind ocamlc -package ocaml-vdom -no-check-prims -linkpkg -o myprog.exe myprog.ml
    js_of_ocaml +gen_js_api/ojs_runtime.js -o myprog.js myprog.exe

The Javascript code can then be used from a simple HTML file such as:

````html
<html>
  <head>
    <script src="myprog.js"></script>
  </head>
  <body>
  </body>
</html>
````

Examples: [`Demo`](examples/demo.ml), [`Counters`](examples/counters.ml)

Third-party examples:
  `TodoMVC`
    ([source](https://github.com/slegrand45/examples_ocaml_vdom/blob/master/todomvc/todomvc.ml),
     [demo](https://slegrand45.github.io/examples_ocaml_vdom.site/todomvc/)),
  [`With Eliom service`](https://github.com/slegrand45/examples_ocsigen/blob/master/eliom/with-ocaml-vdom/simple/mixvdomandeliom.eliom)


About
-----

This project has been created by LexiFi initially for its internal
use.  It is already used in production but it is still relatively new
and no commitment is made on the stability of its interface.  So
please let us know if you consider using it!

This ocaml-vdom package is licensed by LexiFi under the terms of the
[MIT license](LICENSE).

Contact: alain.frisch@lexifi.com
