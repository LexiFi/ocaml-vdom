(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2024 LexiFi                                                                    *)



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
    | Echo of 'msg
    | Batch of 'msg t list
    | Bind: 'a t * ('a -> 'msg t) -> 'msg t
    | Map: ('a -> 'msg) * 'a t -> 'msg t

  val echo: 'msg -> 'msg t
  val batch: 'msg t list -> 'msg t
  val map: ('a -> 'msg) -> 'a t -> 'msg t
  val bind: 'a t -> ('a -> 'msg t) -> 'msg t
end

(* {2 Decoders}*)

type js_object = .. (* forward declaration in Vdom_blit, to avoid depending to DOM API here *)

module Decoder: sig

  type arg_value =
    | StringArg of string
    | BoolArg of bool
    | FloatArg of float
    | IntArg of int

  type _ t =
    | Field : string * 'msg t -> 'msg t
    | Method : string * arg_value list * 'msg t -> 'msg t
    | Bind : ('a -> 'msg t) * 'a t -> 'msg t
    | Const : 'msg -> 'msg t
    | Factor : ('a -> 'msg t) -> ('a -> ('msg, string) Result.t) t
    | String : string t
    | Int : int t
    | Float : float t
    | Bool : bool t
    | Object : js_object t
    | List : 'a t -> 'a list t
    | Fail : string -> 'msg t
    | Try : 'a t -> 'a option t
    (** The type of JavaScript object "structural" parsers.
         It allows to access the fields of JS objects and cast them to OCaml values. *)

  val field: string -> 'a t -> 'a t
  (** [field s d] accesses field [s] and applies decoder [d] to it. Deeper sub-fields can also be accessed by giving the full list of field names separated by dots. *)

  val method_: string -> arg_value list -> 'a t -> 'a t
  (** [method_ s l q] calls method [s] with arguments [l] and applies [d] to the result. *)

  val app: ('a -> 'b) t -> 'a t -> 'b t
  (** Monadic applicative operation *)

  val bind: ('a -> 'b t) -> 'a t -> 'b t
  (** Monadic binding operation *)

  val const: 'a -> 'a t
  (** [const x] is a decoder that always returns [x] on any JS object *)

  val return: 'a -> 'a t
  (** [return x] is a decoder that always returns [x] on any JS object *)

  val factor: ('a -> 'b t) -> ('a -> ('b, string) Result.t) t
  (** [factor f] creates a decoder which returns a function which finishes applying the decoder returned by [f] with a given argument *)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** Monadic mapping operation *)

  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Maps the results of 2 decoders *)

  val pair: 'a t -> 'b t -> ('a * 'b) t
  (** Combine the results of 2 decoders to a pair *)

  val fail: string -> 'a t
  (** [fail msg] is a decoder which always fails with message [msg] *)

  val try_: 'a t -> 'a option t
  (** [try_ d] is a decoder which returns [None] if [d] fails *)

  val string: string t
  (** Decode a JS string to an OCaml string *)

  val int: int t
  (** Decode a JS number to an OCaml integer *)

  val float: float t
  (** Decode a JS number to an OCaml float *)

  val bool: bool t
  (** Decode a JS boolean to an OCaml boolean *)

  val unit: unit t
  (** This decoder always returns () *)

  val object_ : js_object t
  (** Decode a JS object to an OCaml representation *)

  val list: 'a t -> 'a list t
  (** [list d] decodes a JS array to an OCaml list, applying the decoder [d] to each element *)

  val ( let* ): 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ): 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ): 'a t -> 'b t -> ('a * 'b) t

end


(** {2 Custom elements} *)

module Custom: sig
  type t = ..
  type event = ..
end


(** {2 Properties and event handlers} *)

type mouse_event = {x: float; y: float; page_x: float; page_y: float; element_x: float Lazy.t; element_y: float Lazy.t; buttons: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type key_event = {which: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type paste_event = {text: string; selection_start: int; selection_end: int}

type 'msg msg_options = {msg: 'msg option; stop_propagation: bool; prevent_default: bool}

type +'msg event_handler =
  | Decoder : { event_type : string; decoder : 'a msg_options Decoder.t; map : 'a option -> 'msg option } -> 'msg event_handler
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

val on: ?prevent_default:unit -> ?stop_propagation:unit -> string -> 'msg option Decoder.t -> 'msg attribute
val on_with_options: string -> 'msg msg_options Decoder.t -> 'msg attribute
val on_js: ?prevent_default:unit -> ?stop_propagation:unit -> string -> (js_object -> 'msg option) -> 'msg attribute
val on_js_with_options: string -> (js_object -> 'msg msg_options) -> 'msg attribute
val onmousedown: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onmousedown_cancel: ?stop_propagation:unit -> (mouse_event -> 'msg option) -> 'msg attribute
val onmouseup: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onclick: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onclick_cancel: ?stop_propagation:unit -> (mouse_event -> 'msg option) -> 'msg attribute
val ondblclick: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val oncontextmenu: ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onfocus: ?prevent_default:unit -> ?stop_propagation:unit -> 'msg -> 'msg attribute
val onblur: ?prevent_default:unit -> ?stop_propagation:unit -> 'msg -> 'msg attribute
val oninput: ?prevent_default:unit -> ?stop_propagation:unit -> (string -> 'msg) -> 'msg attribute
(** Pass the [value] property of the event target. *)

val onchange_checked: ?prevent_default:unit -> ?stop_propagation:unit -> (bool -> 'msg) -> 'msg attribute
(** Pass the [checked] property of the event targer. *)

val onchange: ?prevent_default:unit -> ?stop_propagation:unit -> (string -> 'msg) -> 'msg attribute
(** Pass the [value] property of the event target. *)

val onchange_index: ?prevent_default:unit -> ?stop_propagation:unit -> (int -> 'msg) -> 'msg attribute
(** Pass the [selected_index] property of the event target. *)

val onmousemove: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onmouseenter: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onmouseleave: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onmouseover: ?prevent_default:unit -> ?stop_propagation:unit -> (mouse_event -> 'msg) -> 'msg attribute
val onkeydown: ?prevent_default:unit -> ?stop_propagation:unit -> (key_event -> 'msg) -> 'msg attribute
val onkeydown_cancel: ?stop_propagation:unit -> (key_event -> 'msg option) -> 'msg attribute
val onkeyup: ?prevent_default:unit -> ?stop_propagation:unit -> (key_event -> 'msg) -> 'msg attribute
val onkeyup_cancel: ?stop_propagation:unit -> (key_event -> 'msg option) -> 'msg attribute
val onpaste: ?prevent_default:unit -> ?stop_propagation:unit -> (paste_event -> 'msg option) -> 'msg attribute
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

val autofocus_prevent_scroll: 'msg attribute
(** When this pseudo-attribute is first applied to an element, the
    element gets focused, but the browser does not scroll to show the element. *)

val autofocus_counter: int -> 'msg attribute
(** When this pseudo-attribute is first applied to an element, or applied
    with a different counter as the previous time, the
    element gets focused. *)

val select: 'msg attribute
(** When this pseudo-attribute is first applied to an input or textarea element,
    select the content. *)

val autosubmit: 'msg attribute
(** When this pseudo_attribute is first applied to a form element,
    it will be submitted automatically. *)


(** {2 VDOM} *)


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
  | Custom of
      {
        key: string;
        elt: Custom.t;
        attributes: 'msg attribute list;
        propagate_events: bool;
      }

(** {3 Generic VDOM constructors} *)

type ('msg, 'res) elt_gen = ?key:string -> ?a:'msg attribute list -> 'res

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

val custom: ?key:string -> ?a:'msg attribute list -> ?propagate_events:unit -> Custom.t -> 'msg vdom
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
