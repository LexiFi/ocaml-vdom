(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2022 LexiFi                                                                    *)


module Cmd = struct
  type +'msg t = ..

  type +'msg t +=
    | Echo of 'msg
    | Batch of 'msg t list
    | Bind: 'a t * ('a -> 'msg t) -> 'msg t
    | Map: ('a -> 'msg) * 'a t -> 'msg t

  let echo msg = Echo msg
  let batch l = Batch l
  let map f x = Map (f, x)
  let bind x f = Bind (x, f)
end

module Custom = struct
  type t = ..
  type event = ..
end

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
  | Attribute of string * string

let onmousedown msg = Handler (MouseDown msg)
let onmousedown_cancel msg = Handler (MouseDownCancel msg)
let onmouseup msg = Handler (MouseUp msg)
let onclick msg = Handler (Click msg)
let onclick_cancel msg = Handler (ClickCancel msg)
let ondblclick msg = Handler (DblClick msg)
let oncontextmenu msg = Handler (ContextMenu msg)
let onfocus msg = Handler (Focus msg)
let oninput msg = Handler (Input msg)
let onchange msg = Handler (Change msg)
let onchange_index msg = Handler (ChangeIndex msg)
let onchange_checked msg = Handler (ChangeChecked msg)
let onblur msg = Handler (Blur msg)
let onmousemove msg = Handler (MouseMove msg)
let onmouseenter msg = Handler (MouseEnter msg)
let onmouseleave msg = Handler (MouseLeave msg)
let onmouseover msg = Handler (MouseOver msg)
let onkeydown msg = Handler (KeyDown msg)
let onkeydown_cancel msg = Handler (KeyDownCancel msg)
let onkeyup msg = Handler (KeyUp msg)
let onkeyup_cancel msg = Handler (KeyUpCancel msg)
let onpaste msg = Handler (Paste msg)
let oncustomevent msg = Handler (CustomEvent msg)


let str_prop k v = Property (k, String v)
let int_prop k v = Property (k, Int v)
let bool_prop k v = Property (k, Bool v)
let float_prop k v = Property (k, Float v)
let style k v = Style (k, v)
let attr k v = Attribute (k, v)
let int_attr k v = Attribute (k, string_of_int v)
let float_attr k v = Attribute (k, string_of_float v)
let scroll_to_show ~align_top = bool_prop "scroll-to-show" align_top
let autofocus = bool_prop "autofocus" true
let autofocus_counter x = int_prop "autofocus" x
let autofocus_if_visible = str_prop "autofocus" "if-visible"
let select = bool_prop "select" true

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

type 'a type_witness = ..

type 'a registered_type = {
  id: int;
  witness: 'a type_witness
}

type (_, _) eq = Refl: ('a, 'a) eq

type type_comparator =
  { cmp: 'a 'b.'a type_witness -> 'b type_witness -> ('a, 'b) eq option }

let registered_type_next_id = ref 0
let registered_types = Hashtbl.create 3

let same_type (type a b) (a : a registered_type) (b : b registered_type) : (a, b) eq option =
  match Hashtbl.find_opt registered_types a.id with
  | Some { cmp } -> cmp a.witness b.witness
  | None -> None

let register_type (type t) () : t registered_type =
  let id = !registered_type_next_id in
  incr registered_type_next_id;
  let module M = struct
    type _ type_witness += Witness: t type_witness
  end
  in
  let cmp: 'a 'b.'a type_witness -> 'b type_witness -> ('a, 'b) eq option =
    fun (type a b) (a: a type_witness) (b : b type_witness) : (a, b) eq option ->
      match a, b with
      | M.Witness, M.Witness -> Some Refl
      | _ -> None
  in
  Hashtbl.add registered_types id { cmp };
  { id ; witness = M.Witness }


type 'a context = {
  context_type: 'a registered_type;
  default_value: 'a;
}
let context_type {context_type; _} = context_type
let create_context default_value = { context_type = register_type (); default_value }
let context_id { context_type; _ } = context_type.id
let context_default_value { default_value; _ } = default_value

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

  | GetContext:
     {
      key: string;
      context: 'a context;
      child: 'a -> 'msg vdom;
     } -> 'msg vdom

  | SetContext:
     {
      key: string;
      context: 'a context;
      value: 'a;
      child: 'msg vdom;
     } -> 'msg vdom

  | Component:
      {
        model_type: 'model registered_type;
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

let text ?(key ="_txt") txt = Text {key; txt}
let fragment ?(key ="_fragment") children = Fragment {key; children}

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

let blur_event = {ev = function Blur msg -> Some msg | _ -> None}
let input_event s = {ev = function Input f -> Some (f s) | _ -> None}
let checked_event b = {ev = function ChangeChecked f -> Some (f b) | _ -> None}
let change_event s = {ev = function Change f -> Some (f s) | _ -> None}
let change_index_event i = {ev = function ChangeIndex f -> Some (f i) | _ -> None}
let custom_event e = {ev = function CustomEvent f -> f e | _ -> None}

let trim_end c s =
  let l = ref (String.length s) in
  while !l > 0 && s.[!l - 1] = c do
    decr l
  done;
  if !l < String.length s then String.sub s 0 !l else s

let replace_char s c x =
  match String.index_opt s c with
  | None -> s
  | Some i0 ->
      let buf = Buffer.create (String.length s) in
      Buffer.add_substring buf s 0 i0;
      for i = i0 to String.length s - 1 do
        let u = s.[i] in
        if u = c then Buffer.add_string buf x
        else Buffer.add_char buf u
      done;
      Buffer.contents buf

let to_html vdom =
  let b = Buffer.create 654 in
  let rec aux: type a. a vdom -> unit = function
    | Text {key=_; txt} -> Buffer.add_string b txt
    | Element {key=_; ns; tag; attributes; children} ->
        let concat_tuple s (x1, x2) = x1 ^ s ^ x2 in
        let attrs, styles =
          List.fold_left
            (fun (attrs, styles) -> function
               | Property (name, value) ->
                   let value =
                     match value with
                     | String s -> s
                     | Int i -> string_of_int i
                     | Float f -> trim_end '.' (string_of_float f)
                     | Bool b -> string_of_bool b
                   in
                   (name, value) :: attrs, styles
               | Style (name, value) ->
                   attrs, (name, value) :: styles
               | Handler _ -> attrs, styles
               | Attribute (name, value) ->
                   (name, value) :: attrs, styles
            ) ([], []) attributes
        in
        let attrs =
          match styles with
          | [] -> attrs
          | styles ->
              let styles =
                List.map (concat_tuple ":") styles
                |> String.concat ";"
              in
              ("style", styles) :: attrs
        in
        let attrs = List.rev attrs in
        let attrs = if ns = "" then attrs else ("xmlns", ns) :: attrs in
        let attrs =
          List.map (fun (k, v) ->
              Printf.sprintf "%s=\"%s\"" k
                (replace_char v '"' "&quote;")
            ) attrs
          |> String.concat " "
        in
        Buffer.add_char b '<';
        Buffer.add_string b tag;
        if attrs <> "" then begin
          Buffer.add_char b ' ';
          Buffer.add_string b attrs
        end;
        if children = [] then
          Buffer.add_string b "/>"
        else begin
          Buffer.add_char b '>';
          List.iter aux children;
          Buffer.add_string b "</";
          Buffer.add_string b tag;
          Buffer.add_char b '>'
        end
    | Fragment {key=_; children} ->
        List.iter aux children
    | Map {key=_; f=_; child} ->
        aux child
    | Memo {key=_; f; arg} ->
        aux (f arg)
    | Component _ | GetContext _ | SetContext _
    | Custom _ -> ()
  in
  aux vdom;
  Buffer.contents b

type 'model component_factory =
  { build: 'priv 'pub. ?key:string -> init:'model ->
      update:('model -> 'priv -> 'model * 'priv Cmd.t * 'pub Cmd.t) ->
      ('model -> 'priv vdom) -> 'pub vdom }

let component_factory () =
  let model_type = register_type () in
  let build ?key ~init ~update view =
    let key =
      match key with
      | Some k -> k
      | None -> "fragment"^(string_of_int model_type.id)
    in
    Component {key; model_type; init; update; view}
  in
  { build }

let ret ?(priv = []) ?(pub = []) model = model, Cmd.batch priv, Cmd.batch pub

let set_context ?key context value child =
  let key =
    match key with
    | None -> "set-context-"^(string_of_int (context_id context))
    | Some key -> key
  in
  SetContext {key; context; child; value}

let get_context ?key context f =
  let key =
    match key with
    | None -> "get-context-"^(string_of_int (context_id context))
    | Some key -> key
  in
  GetContext {key; context; child = f}
