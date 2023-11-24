(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2023 LexiFi                                                                    *)


module Cmd = struct
  type 'msg t = ..

  type 'msg t +=
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

type mouse_event = {x: float; y: float; page_x: float; page_y: float; element_x: float Lazy.t; element_y: float Lazy.t; buttons: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type key_event = {which: int; alt_key: bool; ctrl_key: bool; shift_key: bool}

type paste_event = {text: string; selection_start: int; selection_end: int}

type js_object = .. (* forward declaration in Vdom_blit, to avoid depending to DOM API here *)

module Decoder = struct

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

  let field key d = Field (key, d)

  let method_ name arg d = Method (name, arg, d)

  let bind f d = Bind (f, d)

  let ( let* ) d f = bind f d

  let const msg = Const msg

  let return = const

  let factor f = Factor f

  let map f d = let* msg = d in const (f msg)

  let ( let+ ) d f = map f d

  let pair d1 d2 = let* x1 = d1 in let+ x2 = d2 in (x1, x2)

  let ( and+ ) = pair

  let app f d = let* ff = f in map ff d

  let map2 f d1 d2 = let* x1 = d1 in let+ x2 = d2 in f x1 x2

  let map_some f = map (fun x -> Some (f x))

  let string = String

  let int = Int

  let float = Float

  let bool = Bool

  let unit = const ()

  let object_ = Object

  let list d = List d

  let fail error = Fail error

  let try_ d = Try d

  let mouse_event =
    let+ x = field "clientX" float
    and+ y = field "clientY" float
    and+ left_top = factor (fun () ->
        field "currentTarget" @@
        method_ "getBoundingClientRect" [] @@
        let+ left = field "left" float
        and+ top = field "top" float in
        (left, top))
    and+ page_x = field "pageX" float
    and+ page_y = field "pageY" float
    and+ buttons = field "buttons" int
    and+ alt_key = field "altKey" bool
    and+ ctrl_key = field "ctrlKey" bool
    and+ shift_key = field "shiftKey" bool
    in
    let left_top = Lazy.from_fun left_top in
    let get x =
      match Lazy.force x with
      | Ok x -> x
      | Error err -> failwith err
    in
    let element_x = lazy (x -. fst (get left_top)) in
    let element_y = lazy (y -. snd (get left_top)) in
    {
      x;
      y;
      page_x;
      page_y;
      element_x;
      element_y;
      buttons;
      alt_key;
      ctrl_key;
      shift_key;
    }

  let key_event =
    let+ which = field "which" int
    and+ alt_key = field "altKey" bool
    and+ ctrl_key = field "ctrlKey" bool
    and+ shift_key = field "shiftKey" bool in
    {which; alt_key; ctrl_key; shift_key}

  let paste_event =
    let+ text =
      field "clipboardData"
      @@ method_ "getData" [StringArg "text"] string
    and+ selection_start = field "currentTarget.selectionStart" int
    and+ selection_end = field "currentTarget.selectionEnd" int in
    {text; selection_start; selection_end}

end

type 'msg msg_options = {msg: 'msg option; stop_propagation: bool; prevent_default: bool}

type +'msg event_handler =
  | Decoder : { event_type : string; decoder : 'a msg_options Decoder.t; map : 'a option -> 'msg option } -> 'msg event_handler
  | CustomEvent of (Custom.event -> 'msg option)

type prop_val =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool

type +'msg attribute =
  | Property of string * prop_val
  | Style of string * string
  | Handler of 'msg event_handler
  | Attribute of string * string

let on_with_options event_type decoder =
  Handler (Decoder {event_type; decoder; map = Fun.id})

let on ?prevent_default ?stop_propagation event_type decoder =
  let prevent_default = match prevent_default with Some () -> true | None -> false in
  let stop_propagation = match stop_propagation with Some () -> true | None -> false in
  on_with_options event_type
    Decoder.(let+ msg = decoder in
             {msg; stop_propagation; prevent_default})

let on_cancel ?stop_propagation event_type decoder =
  let stop_propagation = match stop_propagation with Some () -> true | None -> false in
  on_with_options event_type
    Decoder.(let+ msg = decoder in
             match msg with
             | Some _ -> {msg; stop_propagation; prevent_default = true}
             | None ->   {msg; stop_propagation; prevent_default = false})

let on_js_with_options event_type handler =
  on_with_options event_type
    Decoder.(let+ o = object_ in handler o)

let on_js ?prevent_default ?stop_propagation event_type handler =
  let prevent_default = match prevent_default with Some () -> true | None -> false in
  let stop_propagation = match stop_propagation with Some () -> true | None -> false in
  on_js_with_options event_type
    (fun e -> {msg = handler e; stop_propagation; prevent_default})

let onmouseevent ?prevent_default ?stop_propagation type_ msg = on ?prevent_default ?stop_propagation type_ (Decoder.map_some msg Decoder.mouse_event)
let onmouseevent_cancel ?stop_propagation type_ msg = on_cancel ?stop_propagation type_ (Decoder.map msg Decoder.mouse_event)
let onmousedown ?prevent_default ?stop_propagation msg = onmouseevent ?prevent_default ?stop_propagation "mousedown" msg
let onmousedown_cancel ?stop_propagation msg = onmouseevent_cancel ?stop_propagation "mousedown" msg
let onmouseup ?prevent_default ?stop_propagation msg = onmouseevent ?prevent_default ?stop_propagation "mouseup" msg
let onclick ?prevent_default ?stop_propagation msg = onmouseevent ?prevent_default ?stop_propagation "click" msg
let onclick_cancel ?stop_propagation msg = onmouseevent_cancel ?stop_propagation "click" msg
let ondblclick ?prevent_default ?stop_propagation msg = onmouseevent ?prevent_default ?stop_propagation "dblclick" msg
let oncontextmenu ?stop_propagation msg = onmouseevent ~prevent_default:() ?stop_propagation "contextmenu" msg
let onmousemove ?prevent_default ?stop_propagation msg = onmouseevent ?prevent_default ?stop_propagation "mousemove" msg
let onmouseenter ?prevent_default ?stop_propagation msg =
  on ?prevent_default ?stop_propagation "mouseenter" Decoder.(
      let* target = field "target" object_
      and+ current_target = field "currentTarget" object_ in
      if target = current_target then
        map_some msg mouse_event
      else
        const None
    )
let onmouseleave ?prevent_default ?stop_propagation msg =
  on ?prevent_default ?stop_propagation "mouseleave" Decoder.(
      let* target = field "target" object_
      and+ current_target = field "currentTarget" object_ in
      if target = current_target then
        map_some msg mouse_event
      else
        const None
    )
let onmouseover ?prevent_default ?stop_propagation msg = onmouseevent ?prevent_default ?stop_propagation "mouseover" msg

let onfocus ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "focus" (Decoder.const (Some msg))
let onblur ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "blur" (Decoder.const (Some msg))

let oninput ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "input" Decoder.(map_some msg (field "target.value" string))
let onchange ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "change" Decoder.(map_some msg (field "target.value" string))

let onchange_index ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "change" Decoder.(map_some msg (field "target.selectedIndex" int))
let onchange_checked ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "click" Decoder.(map_some msg (field "target.checked" bool))

let onkeyevent ?prevent_default ?stop_propagation type_ msg = on ?prevent_default ?stop_propagation type_ (Decoder.map_some msg Decoder.key_event)
let onkeyevent_cancel ?stop_propagation type_ msg = on_cancel ?stop_propagation type_ (Decoder.map msg Decoder.key_event)
let onkeydown ?prevent_default ?stop_propagation msg = onkeyevent ?prevent_default ?stop_propagation "keydown" msg
let onkeydown_cancel ?stop_propagation msg = onkeyevent_cancel ?stop_propagation "keydown" msg
let onkeyup ?prevent_default ?stop_propagation msg = onkeyevent ?prevent_default ?stop_propagation "keyup" msg
let onkeyup_cancel ?stop_propagation msg = onkeyevent_cancel ?stop_propagation "keyup" msg

let onpaste ?prevent_default ?stop_propagation msg = on ?prevent_default ?stop_propagation "paste" (Decoder.map msg Decoder.paste_event)

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
let autofocus_prevent_scroll = str_prop "autofocus" "prevent-scroll"
let autosubmit = bool_prop "autosubmit" true
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
let custom ?(key ="_custom") ?(a = []) ?propagate_events elt = Custom {key; elt; attributes = a; propagate_events = (propagate_events = Some ())}

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
                   let name =
                     match name with
                     | "className" -> "class"
                     | _ -> name
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
    | Custom _ -> ()
  in
  aux vdom;
  Buffer.contents b
