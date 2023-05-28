(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2022 LexiFi                                                                    *)



(** {1 Virtual DOM} *)


(** A "Virtual application" (or "Elm-like" application) is described by two types:

    - model: the current state of the UI,

    - msg ("messages"): possible state transitions;

    and by the following values:

    - init: the initial state of the application, plus some initial
      commands to be spawned;

    - view: a function mapping the current state to a "virtual" DOM
      tree (vdom);

    - update: a function that processes messages to update the
      current state, and potentially spawns some commands.


    Commands represents (typically) asynchronous operations, such as
    querying a server, or waiting for some timer.

    Messages can be generated either by a VDOM tree (to encapsulate
    DOM events) or by commands (to notify their outcome).
*)

(** {2 Commands} *)

module Cmd: sig
  type +'msg t = ..

  type 'msg t +=
    | Echo of 'msg
    | Batch of 'msg t list
    | Bind: 'a t * ('a -> 'msg t) -> 'msg t
    | Map: ('a -> 'msg) * 'a t -> 'msg t

  val echo: 'msg -> 'msg t
  val batch: 'msg t list -> 'msg t
  val map: ('a -> 'msg) -> 'a t -> 'msg t
  val bind: 'a t -> ('a -> 'msg t) -> 'msg t
end

(** {2 Custom elements} *)

module Custom: sig
  type t = ..
  type event = ..
end


(** {2 Properties and event handlers} *)

type mouse_event = {x: int; y: int; page_x: float; page_y: float; element_x: float; element_y: float; buttons: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type key_event = {which: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type paste_event = {text: string; selection_start: int; selection_end: int}

type 'msg event_handler =
  | MouseDown of (mouse_event -> 'msg)
  | MouseDownCancel of (mouse_event -> 'msg option)
  | MouseUp of (mouse_event -> 'msg)
  | Click of (mouse_event -> 'msg)
  | ClickCancel of (mouse_event -> 'msg option)
  | DblClick of (mouse_event -> 'msg)
  | Focus of 'msg
  | Blur of 'msg
  | Input of (string -> 'msg)
  | Change of (string -> 'msg)
  | ChangeIndex of (int -> 'msg)
  | ChangeChecked of (bool -> 'msg)
  | MouseMove of (mouse_event -> 'msg)
  | MouseEnter of (mouse_event -> 'msg)
  | MouseLeave of (mouse_event -> 'msg)
  | MouseOver of (mouse_event -> 'msg)
  | KeyDown of (key_event -> 'msg)
  | KeyDownCancel of (key_event -> 'msg option)
  | KeyUp of (key_event -> 'msg)
  | KeyUpCancel of (key_event -> 'msg option)
  | ContextMenu of (mouse_event -> 'msg)
  | Paste of (paste_event -> 'msg option)
  | CustomEvent of (Custom.event -> 'msg option)

type prop_val =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool

type 'msg attribute =
  | Property of string * prop_val
  | Style of string * string
  | Handler of 'msg event_handler
  | Attribute of string * string (* use a prop_val to avoid
                                    casting number to strings
                                    when producing the vdom? *)


(** {3 Event handlers} *)

val onmousedown: (mouse_event -> 'msg) -> 'msg attribute
val onmousedown_cancel: (mouse_event -> 'msg option) -> 'msg attribute
val onmouseup: (mouse_event -> 'msg) -> 'msg attribute
val onclick: (mouse_event -> 'msg) -> 'msg attribute
val onclick_cancel: (mouse_event -> 'msg option) -> 'msg attribute
val ondblclick: (mouse_event -> 'msg) -> 'msg attribute
val oncontextmenu: (mouse_event -> 'msg) -> 'msg attribute
val onfocus: 'msg -> 'msg attribute
val onblur: 'msg -> 'msg attribute
val oninput: (string -> 'msg) -> 'msg attribute
(** Pass the [value] property of the event target. *)

val onchange_checked: (bool -> 'msg) -> 'msg attribute
(** Pass the [checked] property of the event targer. *)

val onchange: (string -> 'msg) -> 'msg attribute
(** Pass the [value] property of the event target. *)

val onchange_index: (int -> 'msg) -> 'msg attribute
(** Pass the [selected_index] property of the event target. *)

val onmousemove: (mouse_event -> 'msg) -> 'msg attribute
val onmouseenter: (mouse_event -> 'msg) -> 'msg attribute
val onmouseleave: (mouse_event -> 'msg) -> 'msg attribute
val onmouseover: (mouse_event -> 'msg) -> 'msg attribute
val onkeydown: (key_event -> 'msg) -> 'msg attribute
val onkeydown_cancel: (key_event -> 'msg option) -> 'msg attribute
val onkeyup: (key_event -> 'msg) -> 'msg attribute
val onkeyup_cancel: (key_event -> 'msg option) -> 'msg attribute
val onpaste: (paste_event -> 'msg option) -> 'msg attribute
val oncustomevent: (Custom.event -> 'msg option) -> 'msg attribute


(** {3 Generic DOM properties} *)

(** Generic DOM properties correspond to actual properties on DOM objects.
    The name of these properties is usually the same as the corresponding HTML
    attribute, but not always (e.g. "class" attribute, but "className" property). *)

val str_prop: string -> string -> 'msg attribute
val int_prop: string -> int -> 'msg attribute
val bool_prop: string -> bool -> 'msg attribute
val float_prop: string -> float -> 'msg attribute

val style: string -> string -> 'msg attribute
(** A sub-field of the "style" DOM property. *)

val attr: string -> string -> 'msg attribute
val int_attr: string -> int -> 'msg attribute
val float_attr: string -> float -> 'msg attribute

(** {3 Common DOM properties} *)

val class_: string -> 'msg attribute
val type_: string -> 'msg attribute
val type_button: 'msg attribute
val value: string -> 'msg attribute
val disabled: bool -> 'msg attribute

val add_class: string -> 'msg attribute list -> 'msg attribute list

(** {3 Pseudo-attributes} *)

(** Pseudo-attributes are interpreted in a special way
    by the infrastructure. *)

val scroll_to_show: align_top:bool -> 'msg attribute
(** When this pseudo-attribute is first applied to an element, its
    parent is automatically scrolled (vertically) to show the
    element. *)

val autofocus: 'msg attribute
(** When this pseudo-attribute is first applied to an element, the
    element gets focused. *)

val autofocus_if_visible: 'msg attribute
(** When this pseudo-attribute is first applied to an element, the
    element gets focused if the element is visible in the viewport. *)

val autofocus_counter: int -> 'msg attribute
(** When this pseudo-attribute is first applied to an element, or applied
    with a different counter as the previous time, the
    element gets focused. *)

val select: 'msg attribute
(** When this pseudo-attribute is first applied to an input or textarea element,
    select the content. *)

(** {3 Events} *)

type event = {ev: 'msg. ('msg event_handler -> 'msg option)}

val blur_event: event
val input_event: string -> event
val checked_event: bool -> event
val change_event: string -> event
val change_index_event: int -> event
val custom_event: Custom.event -> event

(** {2 VDOM} *)

type (_, _) eq = Refl: ('a, 'a) eq
type 'a component_id
val register_component_id: unit -> 'a component_id
val same_component: 'a component_id -> 'b component_id -> ('a, 'b) eq option

type +'msg vdom =
  | Text of
      {
        key: string;
        txt: string;
      }
  | Fragment of
      {
        key: string;
        children: 'msg vdom list;
      }
  | Element of
      {
        key: string;
        ns: string;
        tag: string;
        attributes: 'msg attribute list;
        children: 'msg vdom list;
      }
  | Map:
      {
        key: string;
        f: ('submsg -> 'msg);
        child: 'submsg vdom;
      } -> 'msg vdom
  | Memo:
      {
        key: string;
        f: ('a -> 'msg vdom);
        arg: 'a;
      } -> 'msg vdom
  | Component:
      {
        id: 'model component_id;
        key: string;
        init: 'model;
        update: 'model -> 'priv -> 'model * 'priv Cmd.t * 'msg Cmd.t;
        view: 'model -> 'priv vdom;
      } -> 'msg vdom
  | Custom of
      {
        key: string;
        elt: Custom.t;
        attributes: 'msg attribute list;
      }

(** {3 Generic VDOM constructors} *)

type ('msg, 'res) elt_gen = ?key:string -> ?a:'msg attribute list ->  'res

val elt: ?ns:string -> string -> ('msg, 'msg vdom list -> 'msg vdom) elt_gen
(** A generic element. *)

val svg_elt: string -> ('msg, 'msg vdom list -> 'msg vdom) elt_gen
(** A generic element in the SVG namespace. *)

val text: ?key:string -> string -> 'msg vdom
(** A text node. *)

val fragment: ?key:string -> 'msg vdom list -> 'msg vdom
(** A fragment node (not appearing in the dom). *)

val map_attr: ('msg attribute list -> 'msg attribute list) -> 'msg vdom -> 'msg vdom
(** Map attributes of a vdom element *)

val map: ?key:string -> ('a -> 'b) -> 'a vdom -> 'b vdom
(** Wrap all messages generated by a VDOM tree with the provided
    function. *)

val memo: ?key:string -> ('a -> 'msg vdom) -> 'a -> 'msg vdom
(** Apply the function to generate a VDOM tree only if the function
    or its argument have changed (physically) from the previous
    synchronization.

    Note that physical equality is used both for the function and its
    argument.  In particular, this is unlikely to behave as expected
    if the function is defined inline (as an abstraction) or obtained
    by a (partial) function application.  Instead, the functional
    argument should be a simple reference to a globally defined function.

    [TODO: n-ary versions].
*)

val custom: ?key:string -> ?a:'msg attribute list -> Custom.t -> 'msg vdom
(** A custom kind of node.  Usually not used directly. *)

(** {3 Common elements} *)

val div: ('msg, 'msg vdom list -> 'msg vdom) elt_gen
val input: ('msg, 'msg vdom list -> 'msg vdom) elt_gen
val txt_span: ('msg, string -> 'msg vdom) elt_gen


(** {2 Virtual Application} *)

val return: ?c:'msg Cmd.t list -> 'model -> 'model * 'msg Cmd.t

type ('model, 'msg) app =
  {
    init: ('model * 'msg Cmd.t);
    update: ('model -> 'msg -> 'model * 'msg Cmd.t);
    view: ('model -> 'msg vdom);
  }


val app:
  init:('model * 'msg Cmd.t) ->
  update:('model -> 'msg -> 'model * 'msg Cmd.t) ->
  view:('model -> 'msg vdom) ->
  unit ->
  ('model, 'msg) app


val simple_app:
  init:'model ->
  update:('model -> 'msg -> 'model) ->
  view:('model -> 'msg vdom) ->
  unit ->
  ('model, 'msg) app
(** A simple app does not trigger commands. *)

val to_html: 'msg vdom -> string
(** Convert to HTML *)

type 'model component_factory =
  { build: 'priv 'pub. ?key:string ->
    init:'model ->
    update:('model -> 'priv -> 'model * 'priv Cmd.t * 'pub Cmd.t) -> ('model -> 'priv vdom) -> 'pub vdom }

val component_factory: unit -> 'model component_factory

val ret: ?priv:'priv Cmd.t list -> ?pub:'pub Cmd.t list -> 'model -> 'model * 'priv Cmd.t * 'pub Cmd.t
