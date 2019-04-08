(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)


module Cmd = struct
  type 'msg t = ..

  type 'msg t +=
    | Echo of 'msg
    | Batch of 'msg t list
    | Map: ('a -> 'msg) * 'a t -> 'msg t

  let echo msg = Echo msg
  let batch l = Batch l
  let map f x = Map (f, x)
end

module Custom = struct
  type t = ..
end

type mouse_event = {x: int; y: int; page_x: float; page_y: float; buttons: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type key_event = {which: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type 'msg event_handler =
  | Click of (mouse_event -> 'msg)
  | DblClick of (mouse_event -> 'msg)
  | Focus of 'msg
  | Blur of 'msg
  | Input of (string -> 'msg)
  | Change of (string -> 'msg)
  | ChangeIndex of (int -> 'msg)
  | ChangeChecked of (bool -> 'msg)
  | MouseMove of (mouse_event -> 'msg)
  | KeyDown of (key_event -> 'msg)
  | ContextMenu of (mouse_event -> 'msg)

type prop_val =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool

type 'msg attribute =
  | Property of string * prop_val
  | Style of string * string
  | Handler of 'msg event_handler
  | Attribute of string * string

let onclick msg = Handler (Click msg)
let ondblclick msg = Handler (DblClick msg)
let oncontextmenu msg = Handler (ContextMenu msg)
let onfocus msg = Handler (Focus msg)
let oninput msg = Handler (Input msg)
let onchange msg = Handler (Change msg)
let onchange_index msg = Handler (ChangeIndex msg)
let onchange_checked msg = Handler (ChangeChecked msg)
let onblur msg = Handler (Blur msg)
let onmousemove msg = Handler (MouseMove msg)
let onkeydown msg = Handler (KeyDown msg)


let str_prop k v = Property (k, String v)
let int_prop k v = Property (k, Int v)
let bool_prop k v = Property (k, Bool v)
let float_prop k v = Property (k, Float v)
let style k v = Style (k, v)
let attr k v = Attribute (k, v)
let int_attr k v = Attribute (k, string_of_int v)
let float_attr k v = Attribute (k, string_of_float v)
let scroll_to_show = bool_prop "scroll-to-show" true
let autofocus = bool_prop "autofocus" true

let class_ x = Property ("className", String x)
let type_ x = Property ("type", String x)
let type_button = type_ "button"
let value x = Property ("value", String x)
let disabled x = Property ("disabled", Bool x)

let add_class x attrs =
  let has_className =
    List.exists (function Property ("className", _) -> true | _ -> false) attrs
  in
  if has_className then
    List.map (function
        | Property ("className", String s) ->
            Property ("className", String (Printf.sprintf "%s %s" s x))
        | a ->
            a
      ) attrs
  else
    class_ x :: attrs

type +'msg vdom =
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

let text ?(key ="_txt") txt = Text {key; txt}

type ('msg, 'res) elt_gen =
  ?key:string ->
  ?a:'msg attribute list ->
  'res

let elt ?(ns = "") tag ?key ?(a = []) l =
  Element
    {
      key = (match key with None -> tag | Some k -> k);
      ns;
      tag;
      children = l;
      attributes = a;
    }

let svg_ns = "http://www.w3.org/2000/svg"
let svg_elt tag ?key ?a l = elt ~ns:svg_ns tag ?key ?a l

let div ?key ?a l = elt "div" ?key ?a l
let input ?key ?a l = elt "input" ?key ?a l
let txt_span ?key ?a s = elt "span" ?key ?a [text s]

let map_attr f = function
  | Custom ({ attributes; _ } as x) ->
      Custom { x with attributes = f attributes }
  | Element ({ attributes; _ } as x) ->
      Element { x with attributes = f attributes }
  | x -> x

let map ?(key = "_map") f child = Map {key; f; child}
let memo ?(key = "_memo") f arg = Memo {key; f; arg}
let custom ?(key ="_custom") ?(a = []) elt = Custom {key; elt; attributes = a}

let return ?(c = []) model = model, Cmd.batch c

type ('model, 'msg) app =
  {
    init: ('model * 'msg Cmd.t);
    update: ('model -> 'msg -> 'model * 'msg Cmd.t);
    view: ('model -> 'msg vdom);
  }

let app ~init ~update ~view () =
  {init; update; view}

let simple_app ~init ~update ~view () =
  app
    ~init:(return init)
    ~update:(fun model msg -> return (update model msg))
    ~view
    ()


type event = {ev: 'msg. ('msg event_handler -> 'msg option)}

let input_event s = {ev = function Input f -> Some (f s) | _ -> None}
let checked_event b = {ev = function ChangeChecked f -> Some (f b) | _ -> None}
let change_event s = {ev = function Change f -> Some (f s) | _ -> None}
