(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2023 LexiFi                                                                    *)


open Js_browser
open Vdom

let debug = false

type Vdom.js_object += Ojs of Ojs.t

module BDecoder = struct

  type step =
    | Nth of int
    | Key of string

  type typ =
    | TyList
    | TyString
    | TyBool
    | TyInt
    | TyFloat

  type path = step list

  type error_kind =
    | Key_unbound of string
    | Method_unbound of string
    | Msg of string
    | Type_error of typ * string

  type error =
    path * error_kind

  let string_of_typ = function
    | TyList -> "list"
    | TyString -> "string"
    | TyBool -> "boolean"
    | TyInt -> "int"
    | TyFloat -> "float"

  let string_of_step = function
    | Nth n -> string_of_int n
    | Key s -> s

  let string_of_path path =
    String.concat "." (List.rev_map string_of_step path)

  let string_of_error_kind = function
    | Key_unbound s -> Printf.sprintf "unbound field: %S" s
    | Method_unbound s -> Printf.sprintf "unbound method (or wrong number of arguments): %S" s
    | Msg s -> s
    | Type_error (expected, got) -> Printf.sprintf "cannot convert %s to %s" got (string_of_typ expected)

  let string_of_error = function
    | [], errk ->
        string_of_error_kind errk
    | (_ :: _ as path), errk ->
        "At path [" ^ string_of_path path ^ "]: " ^ string_of_error_kind errk

  type o = { path: path; obj: Ojs.t }

  type 'a t = o -> ('a, error) result


  let field key d o =
    let rec aux o = function
      | [] -> Ok o
      | key :: keys ->
          let obj = Ojs.get_prop_ascii o.obj key in
          if obj = Ojs.unit_to_js () then
            Error (o.path, Key_unbound key)
          else aux {path = (Key key :: o.path); obj} keys
    in
    let keys = String.split_on_char '.' key in
    Result.bind (aux o keys) d

  let method_ name arg d o =
    let arg =
      List.map (function
          | Decoder.StringArg s -> Ojs.string_to_js s
          | BoolArg b -> Ojs.bool_to_js b
          | FloatArg f -> Ojs.float_to_js f
          | IntArg i -> Ojs.int_to_js i
        ) arg
    in
    let arg = Array.of_list arg in
    try
      let obj = Ojs.call o.obj name arg in
      d {path = (Key name :: o.path); obj}
    with
      Ojs_exn.Error _ -> Error (o.path, Method_unbound name)


  let bind (f : 'a -> 'b t) (d : 'a t) o =
    match d o with
    | Ok x -> f x o
    | Error err -> Error err

  let const msg _o = Ok msg

  let fail msg {path; _} =
    Error (path, Msg msg)

  let try_ d o =
    match d o with
    | Ok x -> Ok (Some x)
    | Error _ -> Ok None

  let factor (f : ('a -> 'b t)) o =
    Ok (fun x -> Result.map_error string_of_error (f x o))

  let typerr ty path got =
    Error (path, Type_error (ty, got))

  let string {path; obj} =
    match Ojs.type_of obj with
    | "string" -> Ok (Ojs.string_of_js obj)
    | ty -> typerr TyString path ty

  let int {path; obj} =
    match Ojs.type_of obj with
    | "number" -> Ok (Ojs.int_of_js obj)
    | ty -> typerr TyInt path ty

  let float {path; obj} =
    match Ojs.type_of obj with
    | "number" -> Ok (Ojs.float_of_js obj)
    | ty -> typerr TyFloat path ty

  let bool {path; obj} =
    match Ojs.type_of obj with
    | "boolean" -> Ok (Ojs.bool_of_js obj)
    | ty -> typerr TyBool path ty

  let object_ {path=_; obj} = Ok (Ojs obj)

  let mapi_list_result f l =
    let rec loop i acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs -> Result.bind (f i x) (fun x -> loop (i + 1) (x :: acc) xs)
    in
    loop 0 [] l

  let list d o =
    match Ojs.type_of o.obj with
    | "object" ->
        mapi_list_result
          (fun i obj -> d {path = Nth i :: o.path; obj})
          (Ojs.list_of_js Fun.id o.obj)
    | ty -> typerr TyList o.path ty

  let run d obj =
    match d {path = []; obj} with
    | Ok x -> Ok x
    | Error err -> Error (string_of_error err)

  let rec eval : type a. ?extra_fields:(string * Ojs.t) list -> a Decoder.t -> a t = fun ?(extra_fields = []) -> function
    | Field (key, d) ->
        begin match List.assoc_opt key extra_fields with
        | Some obj ->
            fun o -> ((eval d) {path = (Key key :: o.path); obj})
        | None ->
            field key (eval d)
        end
    | Method (name, arg, d) -> method_ name arg (eval d)
    | Bind (f, d) -> bind (fun a -> eval ~extra_fields (f a)) (eval ~extra_fields d)
    | Factor f -> factor (fun a -> eval ~extra_fields (f a))
    | Const msg -> const msg
    | String -> string
    | Int -> int
    | Float -> float
    | Bool -> bool
    | Object -> object_
    | List d -> list (eval d)
    | Fail msg -> fail msg
    | Try d -> try_ (eval ~extra_fields d)

  let decode ?(extra_fields = []) d = run (eval ~extra_fields d)

  let decode_fail ?(extra_fields = []) d o =
    match decode ~extra_fields d o with
    | Ok res -> res
    | Error err -> failwith err
end

module Encoder = struct

  type arg_value =
    | StringArg of string
    | BoolArg of bool
    | FloatArg of float

  type t =
    | Unit
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | List of t list
    | Obj of (string * t) list
    | Fun of (arg_value list -> t)

  let convert_arg o =
    match Ojs.type_of o with
    | "string" -> StringArg (Ojs.string_of_js o)
    | "boolean" -> BoolArg (Ojs.bool_of_js o)
    | "number" -> FloatArg (Ojs.float_of_js o)
    | ty -> failwith ("Encoded function received an argument with unsupported type: " ^ ty)

  let rec encode = function
    | Unit -> Ojs.unit_to_js ()
    | String s -> Ojs.string_to_js s
    | Int i -> Ojs.int_to_js i
    | Float f -> Ojs.float_to_js f
    | Bool b -> Ojs.bool_to_js b
    | List l -> Ojs.list_to_js encode l
    | Obj l -> Ojs.obj (Array.of_list (List.map (fun (s, e) -> (s, encode e)) l))
    | Fun f -> Ojs.fun_to_js_args (fun o -> encode (f (Ojs.list_of_js convert_arg o)))

end


module Cmd = struct
  type 'msg ctx =
    {
      container: Js_browser.Element.t;
      send_msg: ('msg -> unit);
      after_redraw: (unit -> unit) -> unit;
    }

  let container ctx = ctx.container

  let send_msg ctx = ctx.send_msg

  let after_redraw ctx = ctx.after_redraw

  type handler = {f: 'msg. 'msg ctx -> 'msg Vdom.Cmd.t -> bool}

  let rec run: type t. ((unit -> unit) -> unit) -> handler list -> (t -> unit) -> Js_browser.Element.t -> t Cmd.t -> unit =
    fun after h p elt -> function
      | Cmd.Echo msg -> p msg
      | Cmd.Batch l -> List.iter (run after h p elt) l
      | Cmd.Map (f, cmd) -> run after h (fun x -> p (f x)) elt cmd
      | Cmd.Bind (cmd, f) -> run after h (fun x -> run after h p elt (f x)) elt cmd
      | x ->
          let ctx = {send_msg = p; container = elt; after_redraw = after} in
          let rec loop = function
            | [] -> Printf.ksprintf failwith "No command handler found! (%s)" (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val x))
            | hd :: tl ->
                if hd.f ctx x then ()
                else loop tl
          in
          loop h
end

module Custom = struct
  type t =
    {
      dom: Js_browser.Element.t;
      sync: (Vdom.Custom.t -> bool);
      dispose: (unit -> unit);
    }

  type event =
    | Custom of Custom.event
    | Encoded of { event_type: string; encoder: Encoder.t }

  let blur_event = Encoded {event_type = "blur"; encoder = Obj []}
  let input_event s = Encoded {event_type = "input"; encoder = Obj ["target", Obj ["value", String s]]}
  let checked_event b = Encoded {event_type = "click"; encoder = Obj ["target", Obj ["checked", Bool b]]}
  let change_event s = Encoded {event_type = "change"; encoder = Obj ["target", Obj ["value", String s]]}
  let change_index_event i = Encoded {event_type = "change"; encoder = Obj ["target", Obj ["selectedIndex", Int i]]}
  let custom_event e = Custom e

  type ctx =
    {
      parent: Js_browser.Element.t;
      send_event: (event -> unit);
      after_redraw: ((unit -> unit) -> unit);
    }

  type handler = ctx -> Vdom.Custom.t -> t option

  let make ?(dispose = ignore) ~sync dom =
    {dom; sync; dispose}

  let parent ctx = ctx.parent

  let send_event ctx = ctx.send_event

  let after_redraw ctx = ctx.after_redraw

  let rec find_handler ctx x = function
    | [] -> failwith "Vdom_blit: no custom element handler found"
    | hd :: tl ->
        begin match hd ctx x with
        | Some f -> f
        | None -> find_handler ctx x tl
        end

  let lookup ~parent ~process_custom ~after_redraw elt handlers =
    let rec dom = lazy ((Lazy.force el).dom)
    and send_event e = process_custom (Lazy.force dom) e
    and el = lazy (find_handler {parent; send_event; after_redraw} elt handlers) in
    Lazy.force el
end


(* Rendering (VDOM -> DOM) *)

type 'msg ctrl =
  | BText of {vdom: 'msg vdom; dom: Element.t}
  | BFragment of {vdom: 'msg vdom; doms: Element.t list; children: 'msg ctrl list}
  | BElement of {vdom: 'msg vdom; dom: Element.t; children: 'msg ctrl list}
  | BMap: {vdom: 'msg vdom; doms: Element.t list; f: ('submsg -> 'msg); child: 'submsg ctrl} -> 'msg ctrl
  | BMemo: {vdom: 'msg vdom; doms: Element.t list; child: 'msg ctrl} -> 'msg ctrl
  | BCustom of {vdom: 'msg vdom; elt: Custom.t; ns: string; propagate_events: bool}

let get_doms = function
  | BText x -> [x.dom]
  | BFragment x -> x.doms
  | BElement x -> [x.dom]
  | BMap x -> x.doms
  | BMemo x -> x.doms
  | BCustom x -> [x.elt.dom]

let get_vdom = function
  | BText x -> x.vdom
  | BFragment x -> x.vdom
  | BElement x -> x.vdom
  | BMap x -> x.vdom
  | BMemo x -> x.vdom
  | BCustom x -> x.vdom

let key_of_vdom = function
  | Text {key; _}
  | Fragment {key; _}
  | Element {key; _}
  | Map {key; _}
  | Memo {key; _}
  | Custom {key; _} ->
      key


let eval_prop = function
  | String x -> Ojs.string_to_js x
  | Int x -> Ojs.int_to_js x
  | Bool x -> Ojs.bool_to_js x
  | Float x -> Ojs.float_to_js x

let string_of_prop = function
  | String s -> s
  | Int x -> string_of_int x
  | Bool x -> string_of_bool x
  | Float x -> string_of_float x

let same_prop v1 v2 =
  v1 == v2 ||
  match v1, v2 with
  | String x1, String x2 -> x1 = x2
  | Int x1, Int x2 -> x1 = x2
  | Bool x1, Bool x2 -> x1 = x2
  | Float x1, Float x2 -> x1 = x2
  | _ -> false

let bmemo vdom child =
  BMemo {vdom; doms = get_doms child; child}

let is_visible dom =
  let bounding = Element.get_bounding_client_rect dom in
  let top = Rect.top bounding in
  let bottom = Rect.bottom bounding in
  top >= 0.0 && bottom <= Window.inner_height window

let custom_attribute prop =
  match prop with
  | "scroll-to-show" ->
      Some
        (fun dom v ->
           try
             let align_top =
               match v with
               | Bool false -> false
               | _ -> true
             in
             if not (is_visible dom) then
               Element.scroll_into_view dom align_top;
           with exn -> Printf.printf "scroll: %s\n%!" (Printexc.to_string exn)
        )

  | "autofocus" ->
      Some
        (fun dom v ->
           let do_focus =
             match v with
             | String "if-visible" -> is_visible dom
             | _ -> true
           in
           if do_focus then Element.focus dom
        )

  | "select" ->
      Some (fun dom _ -> Element.select dom)

  | "autosubmit" ->
      Some (fun dom _ -> Element.submit dom)

  | _ -> None


let async f =
  ignore (Window.set_timeout window f 0)

let apply_effect_prop dom prop value =
  match custom_attribute prop with
  | None -> false
  | Some f -> async (fun () -> f dom value); true

let set_style dom k v =
  if String.length k > 0 && k.[0] = '-' then
    Ojs.call
      (Ojs.get_prop_ascii (Element.t_to_js dom) "style")
      "setProperty"
      [| Ojs.string_to_js k; v |] |> ignore
  else
    Ojs.set_prop_ascii
      (Ojs.get_prop_ascii (Element.t_to_js dom) "style")
      k
      v

let apply_special_prop ns dom k v =
  match ns, k, v with
  | "http://www.w3.org/2000/svg", "className", String s ->
      let class_obj = Ojs.get_prop_ascii (Element.t_to_js dom) "className" in
      Ojs.set_prop_ascii class_obj "baseVal" (Ojs.string_to_js s);
      true
  | _ -> false

let js_empty_string = Ojs.string_to_js ""

let clear_special_prop ns dom k =
  match ns, k with
  | "http://www.w3.org/2000/svg", "className" ->
      let class_obj = Ojs.get_prop_ascii (Element.t_to_js dom) "className" in
      Ojs.set_prop_ascii class_obj "baseVal" js_empty_string;
      true
  | _ -> false

type env =
  {
    cmds: Cmd.handler list;
    customs: Custom.handler list;
  }

let empty = {cmds = []; customs = []}
let cmd h = {empty with cmds = [h]}
let custom h = {empty with customs = [h]}
let merge envs =
  {
    cmds = List.concat (List.map (fun e -> e.cmds) envs);
    customs = List.concat (List.map (fun e -> e.customs) envs);
  }

let global = ref empty

let register e = global := merge [e; !global]

type ctx =
  {
    process_custom: (Element.t -> Custom.event -> unit);
    custom_handlers: Custom.handler list;
    after_redraw: ((unit -> unit) -> unit);
    add_listener: string -> unit;
  }

let apply_attributes ctx ns dom attributes =
  List.iter
    (function
      | Property (k, v) ->
          if not (apply_special_prop ns dom k v) then
            if not (apply_effect_prop dom k v) then
              Ojs.set_prop_ascii (Element.t_to_js dom) k (eval_prop v)

      | Style (k, v) -> set_style dom k (Ojs.string_to_js v)

      | Attribute (k, v) ->
          Element.set_attribute dom k v

      | Handler Decoder {event_type; _} -> ctx.add_listener event_type

      | Handler CustomEvent _ -> ()
    )
    attributes

let rec blit : 'msg. parent:_ -> ctx -> 'msg vdom -> 'msg ctrl =
  fun ~parent ctx vdom ->
  match vdom with
  | Text {txt; key = _} ->
      BText {vdom; dom = Document.create_text_node document txt}

  | Fragment {children; key = _} ->
      let children = List.map (blit ~parent ctx) children in
      let doms = List.concat_map get_doms children in
      BFragment {vdom; doms; children }

  | Map {f; child; key = _} ->
      let child = blit ~parent ctx child in
      BMap {vdom; doms = get_doms child; f; child}

  | Memo {f; arg; key = _} ->
      bmemo vdom (blit ~parent ctx (f arg))

  | Custom {elt; attributes; key = _; propagate_events} ->
      let elt =
        try Custom.lookup ~parent ~process_custom:ctx.process_custom ~after_redraw:ctx.after_redraw elt (ctx.custom_handlers @ (!global).customs);
        with exn ->
          Printf.printf "Error during vdom Custom %s lookup: %s\n%!"
            (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val elt))
            (Printexc.to_string exn);
          raise exn
      in
      let ns =
        Ojs.option_of_js
          Ojs.string_of_js
          (Ojs.get_prop_ascii (Element.t_to_js elt.dom) "namespaceURI")
        |> Option.value ~default:""
      in
      apply_attributes ctx ns elt.dom attributes;
      BCustom {vdom; elt; ns; propagate_events}

  | Element {ns; tag; children; attributes; key = _} ->
      if debug then Printf.printf "create <%s>\n%!" tag;
      let dom =
        if ns = "" then Document.create_element document tag
        else Document.create_element_ns document ns tag
      in
      let children = List.map (blit ~parent:dom ctx) children in
      List.iter (fun c -> List.iter (Element.append_child dom) (get_doms c)) children;
      apply_attributes ctx ns dom attributes;
      BElement {vdom; dom; children}

let blit ~parent ctx vdom =
  try blit ~parent ctx vdom
  with exn ->
    Printf.printf "Error during vdom blit: %s\n%!" (Printexc.to_string exn);
    raise exn

let sync_props to_string same set clear l1 l2 =
  let sort = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) in
  let l1 = sort l1 and l2 = sort l2 in
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> ()

    | (k1, v1) :: tl1, (k2, _) :: _ when k1 < k2 ->
        if debug then Printf.printf "Property %s unset %s =>\n%!" k1 (to_string v1);
        clear k1 v1;
        loop tl1 l2
    | (k1, v1) :: tl1, [] ->
        if debug then Printf.printf "Property %s unset %s =>\n%!" k1 (to_string v1);
        clear k1 v1;
        loop tl1 []

    | (k1, _) :: _, (k2, v2) :: tl2 when k2 < k1 ->
        if debug then Printf.printf "Property %s set => %s\n%!" k2 (to_string v2);
        set k2 v2;
        loop l1 tl2
    | [], (k2, v2) :: tl2 ->
        if debug then Printf.printf "Property %s set => %s\n%!" k2 (to_string v2);
        set k2 v2;
        loop [] tl2

    | (_k1, v1) :: tl1, (k2, v2) :: tl2 ->
        (* k1 = k2 *)
        if not (same v1 v2) then begin
          if debug then Printf.printf "Property %s changed %s => %s\n%!" k2 (to_string v1) (to_string v2);
          set k2 v2;
        end;
        loop tl1 tl2
  in
  loop l1 l2

let rec choose f = function
  | [] -> []
  | hd :: tl ->
      match f hd with
      | None -> choose f tl
      | Some x -> x :: choose f tl

let js_zero = Ojs.int_to_js 0
let js_false = Ojs.bool_to_js false

let has_own_property o x =
  let open Ojs in
  bool_of_js (call o "hasOwnProperty" [| string_to_js x |])

let sync_attributes ctx ns dom a1 a2 =
  let props = function Property (k, v) -> Some (k, v) | Style _ | Handler _ | Attribute _ -> None in
  let set k v =
    match k, v with
    | "value", String s when s = Element.value dom -> ()
    | _ ->
        if not (apply_special_prop ns dom k v) then
          if not (apply_effect_prop dom k v) then
            Ojs.set_prop_ascii (Element.t_to_js dom) k (eval_prop v)
  in
  let clear k v =
    if custom_attribute k = None then
      if has_own_property (Element.t_to_js dom) k then
        Ojs.delete_prop_ascii (Element.t_to_js dom) k
      else if not (clear_special_prop ns dom k) then
        Ojs.set_prop_ascii (Element.t_to_js dom) k
          begin match v with
          | String _ -> js_empty_string

          | Int _ | Float _ -> js_zero
          | Bool _ -> js_false
          end
  in
  sync_props
    string_of_prop
    same_prop
    set clear
    (choose props a1)
    (choose props a2);

  let styles = function Style (k, v) -> Some (k, String v) | Property _ | Handler _ | Attribute _ -> None in
  let set k v = set_style dom k (eval_prop v)in
  let clear k _ = set_style dom k js_empty_string in
  sync_props
    string_of_prop
    same_prop
    set clear
    (choose styles a1)
    (choose styles a2);

  let attrs = function Attribute (k, v) -> Some (k, v) | Style _ | Property _ | Handler _ -> None in
  let set k v = Element.set_attribute dom k v in
  let clear k _ = Element.remove_attribute dom k in
  sync_props
    Fun.id
    (fun (s1: string) s2 -> s1 = s2)
    set clear
    (choose attrs a1)
    (choose attrs a2);

  List.iter
    (function | Handler Decoder {event_type; _} -> ctx.add_listener event_type
              | _ -> ())
    a2

let rec dispose : type msg. msg ctrl -> unit = fun ctrl ->
  match ctrl with
  | BText _ -> ()
  | BCustom {elt; _} -> elt.dispose ()
  | BFragment {children; _}
  | BElement {children; _} -> List.iter dispose children
  | BMap {child; _} -> dispose child
  | BMemo {child; _} -> dispose child

let print_element node =
  if Element.null == node then
    "null"
  else
    match Element.node_type node with
    | Element.ELEMENT_NODE -> Element.outer_HTML node
    | Element.TEXT_NODE -> Element.node_value node
    | _ -> "??"

let remove_child parent child =
  if debug then
    Printf.printf "remove_child(%s, %s)\n" (print_element parent) (print_element child);
  Element.remove_child parent child

let replace_child parent n o =
  if debug then
    Printf.printf "replace_child(%s, %s, %s)\n" (print_element parent) (print_element n) (print_element o);
  Element.replace_child parent n o

let insert_before parent o n =
  if debug then
    Printf.printf "insert_before(%s, %s, %s)\n" (print_element parent) (print_element o) (print_element n);
  Element.insert_before parent o n

let rec sync : type old_msg msg. ctx -> Element.t -> bool -> Element.t -> old_msg ctrl -> msg vdom -> msg ctrl =
  fun ctx parent prev_move next old vdom ->

  match old, vdom with
  | _ when (vdom : msg vdom) == (Obj.magic (get_vdom old : old_msg vdom)) ->
      (* If old and new vdom are physically identical, it is safe to reuse the old ctrl. *)
      (Obj.magic (old : old_msg ctrl) : msg ctrl)
  | BText {vdom = Text {txt = s1; key = _}; dom}, Text {txt = s2; key = _} ->
      if s1 <> s2 then Element.set_node_value dom s2;
      BText {vdom; dom}

  | BMap {child = c1; _}, Map {f; child = c2; key = _} ->
      let child = sync ctx parent prev_move next c1 c2 in
      BMap {vdom; doms = get_doms child; child; f}

  | BMemo {child = c1; vdom = Memo {f = f1; arg = a1; key = _}; _}, Memo {f = f2; arg = a2; key = _} ->
      (* Is this safe !? *)
      if Obj.magic f1 == f2 && Obj.magic a1 == a2 then
        bmemo vdom (Obj.magic (c1 : old_msg ctrl) : msg ctrl)
      else
        bmemo vdom (sync ctx parent prev_move next c1 (f2 a2))

  | BCustom {vdom = Custom {key=key1; elt=arg1; attributes=a1; propagate_events = _}; propagate_events = _; elt; ns}, Custom {key=key2; elt=arg2; attributes=a2; propagate_events}
    when key1 = key2 && (arg1 == arg2 || elt.sync arg2) ->
      sync_attributes ctx ns elt.dom a1 a2;
      BCustom {vdom; elt; ns; propagate_events}

  | BFragment {vdom = Fragment e1; children; _}, Fragment e2 when e1.key = e2.key ->
      let children = sync_children ctx parent prev_move next children e2.children in
      let doms = List.concat_map get_doms children in
      BFragment {vdom; doms; children }

  | BElement {vdom = Element e1; dom; children}, Element e2 when e1.tag = e2.tag && e1.ns = e2.ns && e1.key = e2.key ->
      let children = sync_children ctx dom false Element.null children e2.children in
      (* synchronize properties & styles *)
      sync_attributes ctx e1.ns dom e1.attributes e2.attributes;
      BElement {vdom; dom; children}

  | _ ->
      let x = blit ~parent ctx vdom in
      let rec loop l1 l2 =
        match l1, l2 with
        | h1::t1, h2::t2 ->
            replace_child parent h1 h2;
            loop t1 t2
        | [], l ->
            List.iter (remove_child parent) l
        | l, [] ->
            List.iter (fun c -> insert_before parent c next) l
      in
      loop (get_doms x) (get_doms old);
      dispose old;
      x

and sync_children : type old_msg msg. ctx -> Element.t -> bool -> Element.t -> old_msg ctrl list -> msg vdom list -> msg ctrl list =
  fun ctx dom prev_move next old_children new_children ->
  (* TODO:
     - add a fast-path to deal with prefixes and suffixes of old/new children with identical
       keys, avoiding a lot of allocations.
     - use a JS object (map) instead of an OCaml Hashtbl?
  *)

  (* synchronize children *)

  let old_children = Array.of_list old_children in
  let new_children = Array.of_list new_children in

  (* for each key, get a list of indices in old_children *)
  let by_key = Hashtbl.create 8 in
  for i = Array.length old_children - 1 downto 0 do
    let k = key_of_vdom (get_vdom old_children.(i)) in
    Hashtbl.add by_key k i
    (* early elements are inserted last: found first! *)
  done;

  (* build an array telling us, for each position in the "new" sequence,
     where to pick it from the "old" sequence. *)
  let indices = Array.make (Array.length new_children) (-1) in
  for i = 0 to Array.length indices - 1 do
    let k = key_of_vdom new_children.(i) in
    match Hashtbl.find by_key k with
    | exception Not_found -> () (* keep (-1) == need to build from scratch *)
    | idx ->
        indices.(i) <- idx;
        Hashtbl.remove by_key k;
  done;

  Hashtbl.iter
    (fun _ i ->
       if debug then Printf.printf "remove %i\n%!" i;
       let to_remove = old_children.(i) in
       List.iter (remove_child dom) (get_doms to_remove);
       dispose to_remove
    )
    by_key;

  (* produce the new sequence, from right-to-left, creating and picking+syncinc nodes *)
  let ctrls = ref [] in
  let prev_move = ref prev_move in
  let next = ref next in
  for i = Array.length new_children - 1 downto 0 do
    let idx = indices.(i) in
    if debug then Printf.printf "old = %i; new = %i: " idx i;

    let c =
      if idx < 0 then begin
        (* create *)
        if debug then Printf.printf "create\n%!";
        blit ~parent:dom ctx new_children.(i)
      end
      else begin
        if debug then Printf.printf "sync&move\n%!";
        (* note: the sync could lead to a DOM replace,
           following by a move below; in that case,
           one should just delete old + insert new *)
        sync ctx dom !prev_move !next old_children.(idx) new_children.(i)
      end
    in
    (* when next == null, insert at the end *)

    (* We try to avoid useless moves as much as possible, not only for performance reason,
       but also because it causes the loss of focus on input fields.

       See: https://github.com/google/incremental-dom/issues/237

       Better solutions:
       - Restore the focus after the entire synchronization (React seems to do that).
       - Never move a focused widget or one of its ancestors (require a less regular
         algorithm to apply the desired permutation).
    *)
    let doms = get_doms c in
    let rec last = function
      | [] -> None
      | [ c_dom ] -> Some c_dom
      | _ :: tl -> last tl
    in
    match last doms with
    | None -> ()
    | Some right_most ->
        begin
          let move =
            idx < 0 ||
            ((if i = Array.length new_children - 1 then idx <> Array.length old_children - 1
              else !prev_move || indices.(i + 1) <> idx + 1)
             && Element.next_sibling right_most != !next)(* could avoid reading from the DOM... *)
          in
          if move then begin
            if debug then Printf.printf "really move\n%!";
            List.iter (fun c_dom ->
                insert_before dom c_dom !next) doms;
          end;
          prev_move := move;
          next := List.hd doms;
        end;
        ctrls := c :: !ctrls
  done;
  !ctrls

let sync ctx parent old vdom =
  try sync ctx parent old vdom
  with exn ->
    Printf.printf "Error during vdom sync: %s\n%!" (Printexc.to_string exn);
    raise exn

type 'msg find =
  | NotFound
  | Found: {mapper: ('inner_msg -> 'msg); inner: 'inner_msg ctrl; parent: 'msg find} -> 'msg find

let rec found: type inner_msg msg. (inner_msg -> msg) -> msg find -> Element.t -> inner_msg ctrl -> msg find =
  fun mapper parent dom -> function
    | BElement _ | BText _ | BCustom _ as inner -> Found {mapper; inner; parent}
    | BFragment {children; _} ->
        begin match List.find (fun c -> List.memq dom (get_doms c)) children with
        | exception Not_found -> assert false
        | c -> found mapper parent dom c
        end
    | BMap {f; child; _} -> found (fun x -> mapper (f x)) parent dom child
    | BMemo {child; _} -> found mapper parent dom child

(* Find a ctrl associated to a DOM element.
   Normalize by traversing Map node, and also return the composition of all such mappers
   from the root to the ctrl. *)

let rec vdom_of_dom: type msg. msg ctrl -> Element.t -> msg find = fun root dom ->
  (* hack to check dom == null?   Should move that to Ojs. *)
  match Ojs.option_of_js Element.t_of_js (Element.t_to_js dom) with
  | None -> NotFound
  | Some dom when List.memq dom (get_doms root) ->
      found Fun.id NotFound dom root
  | Some dom ->
      begin match vdom_of_dom root (Element.parent_node dom) with
      | NotFound -> NotFound
      | Found {mapper; inner = BElement {children; _}; _} as parent ->
          begin match List.find (fun c -> List.memq dom (get_doms c)) children with
          | exception Not_found -> NotFound
          | c -> found mapper parent dom c
          end
      | Found {mapper; inner = BCustom {propagate_events; _} as inner; parent} ->
          if propagate_events then
            found mapper parent dom inner
          else
            NotFound
      | _ -> assert false
      end

type ('model, 'msg) app = {
  dom: Js_browser.Element.t;
  process: ('msg -> unit);
  get: (unit -> 'model);
  after_redraw: (unit -> unit) -> unit;
  dispose: (unit -> unit);
}

let dom x = x.dom
let process x = x.process
let get x = x.get ()
let after_redraw x = x.after_redraw


let run (type msg model) ?(env = empty) ?container
    ({init = (model0, cmd0); update; view} : (model, msg) Vdom.app) =
  let env = merge [env; !global] in
  let container_created, container =
    match container with
    | None -> true, Document.create_element document "div"
    | Some container -> false, container
  in
  let post_redraw = ref [] in
  let after_redraw f = post_redraw := f :: !post_redraw in
  let flush _ =
    let l = List.rev !post_redraw in
    post_redraw := [];
    List.iter (fun f -> f ()) l
  in

  let process_custom_fwd = ref (fun _ _ -> assert false) in
  let process_fwd = ref (fun _ -> assert false) in

  let listeners = Hashtbl.create 8 in
  let model = ref model0 in
  let current = ref None in

  let pending_redraw = ref false in

  let view model =
    try view model
    with exn ->
      Printf.printf "Error during vdom view: %s\n%!" (Printexc.to_string exn);
      raise exn
  in

  let on_event evt =
    let ty = Event.type_ evt in
    try
      let tgt = Element.t_of_js (Event.target evt) in
      let apply_handler dom =
        List.filter_map (fun attribute ->
            match attribute with
            | Handler (Decoder {event_type; decoder; map}) when ty = event_type ->
                let {msg; prevent_default; stop_propagation} =
                  BDecoder.decode_fail
                    ~extra_fields:["currentTarget", Element.t_to_js dom]
                    decoder
                    (Event.t_to_js evt)
                in
                if prevent_default then Event.prevent_default evt;
                if stop_propagation then Event.stop_propagation evt;
                begin match map msg with
                | None -> None
                | Some msg ->
                    Some (msg, stop_propagation)
                end
            | _ -> None)
      in
      let rec propagate = function
        | Found {
            mapper;
            inner = ( BElement {vdom = Element {attributes; _}; dom; _}
                    | BCustom  {vdom = Custom  {attributes; _}; elt = {dom; _}; _} );
            parent;
          } ->
            let stop_propagation =
              List.fold_left
                (fun stopped_propagation (msg, stop_propagation) ->
                   !process_fwd (mapper msg);
                   stopped_propagation || stop_propagation)
                false
                (apply_handler dom attributes)
            in
            if not stop_propagation then propagate parent
        | _ ->
            ()
      in
      Option.iter (fun root ->
          propagate (vdom_of_dom root tgt);
        ) !current;

      if ty = "input" || ty = "blur" then
        let f () =
          Option.iter
            (fun root ->
               match vdom_of_dom root tgt with
               (* note: the new vdom can be different after processing
                  the event above *)
               (* !! This is probably broken now that we delay updating the vdom
                     with request_animation_frame !! *)
               | Found {mapper = _; inner = BElement {vdom = Element {attributes; _}; _}; _} ->
                   List.iter
                     (function
                       | Property ("value", String s2) when s2 <> Element.value tgt -> Element.set_value tgt s2
                       | Property ("checked", Bool s2) -> Element.set_checked tgt s2
                       | _ -> ()
                     )
                     attributes
               | _ -> ()
            ) !current
        in
        if !pending_redraw then after_redraw f else f ()
    with exn ->
      Printf.printf "Error in event handler %S: %s\n%!" ty (Printexc.to_string exn)
  in

  let add_listener event_type =
    if not (Hashtbl.mem listeners event_type) then begin
      if debug then Printf.printf "Adding listener for %s\n%!" event_type;
      let remove_listener = Element.add_cancellable_event_listener container (Event.NonStandard event_type) on_event true in
      Hashtbl.add listeners event_type remove_listener
    end
  in

  let ctx =
    {
      process_custom = (fun elt evt -> !process_custom_fwd elt evt);
      custom_handlers = env.customs;
      after_redraw;
      add_listener;
    }
  in
  let x = blit ~parent:container ctx (view model0) in
  Window.request_animation_frame window flush;

  current := Some x;

  let redraw _ =
    (* TODO:
       could avoid calling view/sync if model is the same as the previous one
       (because updates are now batched
    *)
    match !current with
    | None -> ()
    | Some root ->
        pending_redraw := false;
        let x = sync ctx container false Element.null root (view !model) in
        current := Some x;
        flush ()
  in

  let rec process msg =
    try
      let (new_model : model), (cmd : msg Vdom.Cmd.t) = update !model msg in
      model := new_model;
      run_cmd container cmd;
      if not !pending_redraw then begin
        pending_redraw := true;
        Window.request_animation_frame window redraw
      end
    with exn ->
      Printf.printf "Error during vdom process: %s\n%!" (Printexc.to_string exn);
      raise exn
  and run_cmd (parent : Js_browser.Element.t) cmd =
    Cmd.run after_redraw (env.cmds @ (!global).cmds) process parent cmd
  in

  process_fwd := process;


  List.iter (Element.append_child container) (get_doms x);

  let process_custom tgt event =
    Option.iter
      (fun root ->
         let process mapper attributes =
           let select_handler attr =
             match attr, event with
             | Handler (Decoder {decoder; map; event_type = et1}), Custom.Encoded {encoder; event_type = et2} when et1 = et2 ->
                 let {msg; _} = BDecoder.decode_fail decoder (Encoder.encode encoder) in map msg
             | Handler (CustomEvent f), Custom e -> f e
             | _ -> None
           in
           let msgs = List.filter_map select_handler attributes in
           List.iter (fun msg -> process (mapper msg)) msgs
         in
         begin match vdom_of_dom root tgt with
         | Found {mapper; inner = BElement {vdom = Element {attributes; _}; _}; _} ->
             process mapper attributes
         | Found {mapper; inner = BCustom  {vdom = Custom  {attributes; _}; _}; _} ->
             process mapper attributes
         | _ ->
             ()
         end
      ) !current
      (* Do we need to do something similar to the "input" case in on_event? *)
  in
  process_custom_fwd := process_custom;

  run_cmd container cmd0;
  let dispose () =
    Option.iter
      (fun root ->
         current := None;
         dispose root;
         Hashtbl.iter (fun _ f -> f ()) listeners;
         if container_created then
           Element.remove container
         else
           Element.set_inner_HTML container ""
      ) !current
  in
  {dom = container; process; get = (fun () -> !model); after_redraw; dispose}

let dispose {dispose; _} = dispose ()
