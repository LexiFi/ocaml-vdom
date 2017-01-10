(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)



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
  type 'msg t = ..

  type 'msg t +=
    | Batch of 'msg t list
    | Map: ('a -> 'msg) * 'a t -> 'msg t

  val batch: 'msg t list -> 'msg t
  val map: ('a -> 'msg) -> 'a t -> 'msg t
end

(** {2 Custom elements} *)

module Custom: sig
  type t = ..
end


(** {2 Properties and event handlers} *)

type mouse_event = {x: int; y: int; buttons: int}

type key_event = {which: int}

type 'msg event_handler =
  | Click of 'msg
  | DblClick of 'msg
  | Focus of 'msg
  | Blur of 'msg
  | Input of (string -> 'msg)
  | Change of (string -> 'msg)
  | ChangeIndex of (int -> 'msg)
  | MouseMove of (mouse_event -> 'msg)
  | KeyDown of (key_event -> 'msg)

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

val onclick: 'msg -> 'msg attribute
val ondblclick: 'msg -> 'msg attribute
val onfocus: 'msg -> 'msg attribute
val onblur: 'msg -> 'msg attribute
val oninput: (string -> 'msg) -> 'msg attribute
    (** Pass the [value] property of the event target. *)

val onchange: (string -> 'msg) -> 'msg attribute
    (** Pass the [value] property of the event target. *)

val onchange_index: (int -> 'msg) -> 'msg attribute
    (** Pass the [selected_index] property of the event target. *)

val onmousemove:  (mouse_event -> 'msg) -> 'msg attribute
val onkeydown: (key_event -> 'msg) -> 'msg attribute

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

val attr: string -> string -> 'mg attribute
val int_attr: string -> int -> 'msg attribute
val float_attr: string -> float -> 'msg attribute

(** {3 Common DOM properties} *)

val class_: string -> 'msg attribute
val type_: string -> 'msg attribute
val type_button: 'msg attribute
val value: string -> 'msg attribute
val disabled: bool -> 'msg attribute

(** {3 Pseudo-attributes} *)

(** Pseudo-attributes are interpreted in a special way
    by the infrastructure. *)

val scroll_to_show: 'msg attribute
(** When this pseudo-attribute is first applied to an element, its
    parent is automatically scrolled (vertically) to show the
    element. *)

val autofocus: 'msg attribute
(** When this pseudo-attribute is first applied to an element, the
    element gets focused. *)

(** {3 Events} *)

type event = {ev: 'msg. ('msg event_handler -> 'msg option)}
val input_event: string -> event

(** {2 VDOM} *)


type 'msg vdom =
  | Text of
      {
        key: string;
        txt: string;
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

val map: ?key:string -> ('a -> 'b) -> 'a vdom -> 'b vdom
(** Wrap all messages generated by a VDOM tree with the provided
    function. *)

val memo: ?key:string -> ('a -> 'msg vdom) -> 'a -> 'msg vdom
(** Apply the function to generate a VDOM tree only if the function
    or its argument have changed (physically) from the previous
    synchronization.

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
