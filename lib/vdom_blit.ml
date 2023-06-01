(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2022 LexiFi                                                                    *)

open Js_browser
open Vdom

let debug = false

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
            | [] -> Printf.ksprintf failwith "No command handler found! (%s)"
                      (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val x))
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

  type ctx =
    {
      parent: Js_browser.Element.t;
      send_event: (Vdom.event -> unit);
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
  | BGetContext of { vdom: 'msg vdom; doms: Element.t list; child: 'msg ctrl }
  | BSetContext of { vdom: 'msg vdom; doms: Element.t list; child: 'msg ctrl }
  | BComponent:
      {vdom: 'msg vdom; doms: Element.t list; model_type: 'model registered_type;
       state: 'model ref;
       child: 'priv ctrl;
       process: 'priv -> unit;
      } -> 'msg ctrl
  | BMap: {vdom: 'msg vdom; doms: Element.t list; f: ('submsg -> 'msg); child: 'submsg ctrl} -> 'msg ctrl
  | BMemo: {vdom: 'msg vdom; doms: Element.t list; child: 'msg ctrl} -> 'msg ctrl
  | BCustom of {vdom: 'msg vdom; elt: Custom.t}

let get_doms = function
  | BText x -> [x.dom]
  | BFragment x -> x.doms
  | BElement x -> [x.dom]
  | BMap x -> x.doms
  | BComponent x -> x.doms
  | BSetContext x -> x.doms
  | BGetContext x -> x.doms
  | BMemo x -> x.doms
  | BCustom x -> [x.elt.dom]

let get_vdom = function
  | BText x -> x.vdom
  | BFragment x -> x.vdom
  | BElement x -> x.vdom
  | BMap x -> x.vdom
  | BMemo x -> x.vdom
  | BCustom x -> x.vdom
  | BComponent x -> x.vdom
  | BGetContext x -> x.vdom
  | BSetContext x -> x.vdom

let key_of_vdom = function
  | Text {key; _}
  | Fragment {key; _}
  | Element {key; _}
  | Map {key; _}
  | Memo {key; _}
  | Component {key; _}
  | Custom {key; _}
  | GetContext {key; _}
  | SetContext {key; _} ->
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

let async f =
  ignore (Window.set_timeout window f 0)

let is_visible dom =
  let bounding = Element.get_bounding_client_rect dom in
  let top = Rect.top bounding in
  let bottom = Rect.bottom bounding in
  top >= 0.0 && bottom <= Window.inner_height window

let custom_attribute dom prop v =
  match prop with
  | "scroll-to-show" ->
      async
        (fun () ->
           try
             let align_top =
               match v with
               | Bool false -> false
               | _ -> true
             in
             if not (is_visible dom) then
               Element.scroll_into_view dom align_top;
           with exn -> Printf.printf "scroll: %s\n%!" (Printexc.to_string exn)
        );
      true

  | "autofocus" ->
      async (fun () ->
          let do_focus =
            match v with
            | String "if-visible" -> is_visible dom
            | _ -> true
          in
          if do_focus then Element.focus dom
        );
      true

  | "select" ->
      async (fun () -> Element.select dom);
      true

  | _ -> false



let apply_attributes dom attributes =
  List.iter
    (function
      | Property (k, v) ->
          if not (custom_attribute dom k v) then
            Ojs.set_prop_ascii (Element.t_to_js dom) k (eval_prop v)

      | Style (k, v) ->
          Ojs.set_prop_ascii
            (Ojs.get_prop_ascii (Element.t_to_js dom) "style")
            k
            (Ojs.string_to_js v)

      | Attribute (k, v) ->
          Element.set_attribute dom k v

      | _ -> ()
    )
    attributes

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

module IntMap = Map.Make(Int)
type context_value =
  | Context_value: ('a context * 'a) -> context_value

type 'msg ctx =
  {
    process_custom: (Element.t -> event -> unit);
    custom_handlers: Custom.handler list;
    redraw: unit -> unit;
    after_redraw: ((unit -> unit) -> unit);
    run_cmd: 'msg. ('msg -> unit) -> Element.t -> 'msg Vdom.Cmd.t -> unit;
    process: 'msg -> unit;
    contexts: context_value IntMap.t;
  }

let peek_context (type t) (ctx : _ ctx) (context : t context) : t =
  match IntMap.find_opt (context_id context) ctx.contexts with
  | Some (Context_value (context', value)) ->
    begin match same_type (context_type context) (context_type context') with
    | Some Refl -> value
    | None -> assert false
    end
  | None -> context_default_value context

let push_context ctx context value =
  let id = context_id context in
  let contexts = IntMap.add id (Context_value (context, value)) ctx.contexts in
  { ctx with contexts }

let rec process_component ctx parent state update msg =
  let model, priv, pub = update !state msg in
  state := model;
  ctx.run_cmd (process_component ctx parent state update) parent priv;
  ctx.run_cmd ctx.process parent pub;
  ctx.redraw ()

let rec blit : 'msg. parent:_ -> 'msg ctx -> 'msg vdom -> 'msg ctrl =
  fun ~parent ctx vdom ->
  match vdom with
  | Text {txt; key = _} ->
      BText {vdom; dom = Document.create_text_node document txt}

  | Fragment {children; key = _} ->
      let children = List.map (blit ~parent ctx) children in
      let doms = List.concat_map get_doms children in
      BFragment { vdom; doms; children }

  | Component {key = _; model_type; init; view; update } ->
      let state = ref init in
      let process = process_component ctx parent state update in
      let child = blit ~parent { ctx with process } (view init) in
      BComponent { vdom; model_type; state; process; doms = get_doms child; child }

  | GetContext {context; child; key = _} ->
    let arg = peek_context ctx context in
    let child = blit ~parent ctx (child arg) in
    BGetContext {vdom; doms = get_doms child; child }

  | SetContext {context; child; value; key = _} ->
    let ctx = push_context ctx context value in
    let child = blit ~parent ctx child in
    BSetContext {vdom; doms = get_doms child; child }

  | Map {f; child; key = _} ->
      let process msg = ctx.process (f msg) in
      let child = blit ~parent { ctx with process } child in
      BMap {vdom; doms = get_doms child; f; child}

  | Memo {f; arg; key = _} ->
      bmemo vdom (blit ~parent ctx (f arg))

  | Custom {elt; attributes; key = _} ->
      let elt =
        try Custom.lookup ~parent ~process_custom:ctx.process_custom ~after_redraw:ctx.after_redraw elt (ctx.custom_handlers @ (!global).customs);
        with exn ->
          Printf.printf "Error during vdom Custom %s lookup: %s\n%!"
            (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val elt))
            (Printexc.to_string exn);
          raise exn
      in
      apply_attributes elt.dom attributes;
      BCustom {vdom; elt}

  | Element {ns; tag; children; attributes; key = _} ->
      if debug then Printf.printf "create <%s>\n%!" tag;
      let dom =
        if ns = "" then Document.create_element document tag
        else Document.create_element_ns document ns tag
      in
      let children = List.map (blit ~parent:dom ctx) children in
      List.iter (fun c -> List.iter (Element.append_child dom) (get_doms c)) children;
      apply_attributes dom attributes;
      BElement {vdom; dom; children}

let blit ~parent ctx vdom =
  try blit ~parent ctx vdom
  with exn ->
    Printf.printf "Error during vdom blit: %s\n%!" (Printexc.to_string exn);
    raise exn

let sync_props to_string same set clear l1 l2 =
  let sort = List.sort (fun (k1, _) (k2, _) -> compare (k1:string) k2) in
  let l1 = sort l1 and l2 = sort l2 in
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> ()

    | (k1, v1) :: tl1, (k2, _) :: _ when k1 < k2 ->
        if debug then Printf.printf "Property %s unset %s =>\n%!" k1 (to_string v1);
        clear k1;
        loop tl1 l2
    | (k1, v1) :: tl1, [] ->
        if debug then Printf.printf "Property %s unset %s =>\n%!" k1 (to_string v1);
        clear k1;
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

let js_empty_string =
  Ojs.string_to_js ""

let sync_attributes dom a1 a2 =
  let props = function Property (k, v) -> Some (k, v) | Style _ | Handler _ | Attribute _ -> None in
  let set k v =
    match k, v with
    | "value", String s when s = Element.value dom -> ()
    | _ ->
        if not (custom_attribute dom k v) then
          Ojs.set_prop_ascii (Element.t_to_js dom) k (eval_prop v)
  in
  let clear k = Ojs.set_prop_ascii (Element.t_to_js dom) k Ojs.null in
  sync_props
    string_of_prop
    same_prop
    set clear
    (choose props a1)
    (choose props a2);

  let styles = function Style (k, v) -> Some (k, String v) | Property _ | Handler _ | Attribute _ -> None in
  let set k v = Ojs.set_prop_ascii (Ojs.get_prop_ascii (Element.t_to_js dom) "style") k (eval_prop v) in
  let clear k = Ojs.set_prop_ascii (Ojs.get_prop_ascii (Element.t_to_js dom) "style") k js_empty_string in
  sync_props
    string_of_prop
    same_prop
    set clear
    (choose styles a1)
    (choose styles a2);

  let attrs = function Attribute (k, v) -> Some (k, v) | Style _ | Property _ | Handler _ -> None in
  let set k v = Element.set_attribute dom k v in
  let clear k = Element.remove_attribute dom k in
  sync_props
    Fun.id
    (fun (s1: string) s2 -> s1 = s2)
    set clear
    (choose attrs a1)
    (choose attrs a2)

let rec dispose : type msg. msg ctrl -> unit = fun ctrl ->
  match ctrl with
  | BText _ -> ()
  | BCustom {elt; _} -> elt.dispose ()
  | BFragment {children; _}
  | BElement {children; _} -> List.iter dispose children
  | BMap {child; _} -> dispose child
  | BComponent {child; _} -> dispose child
  | BSetContext {child; _}
  | BGetContext {child; _}
  | BMemo {child; _} -> dispose child

let print_element node =
  if Element.null == node then
    "null"
  else
    Element.outer_HTML node

let remove_child parent child =
  if debug then
    Printf.printf "remove_child(%s, %s)\n" (print_element parent) (print_element child);
  Element.remove_child parent child

let replace_child parent o n =
  if debug then
    Printf.printf "replace_child(%s, %s, %s)\n" (print_element parent) (print_element o) (print_element n);
  Element.replace_child parent o n

let insert_before parent o n =
  if debug then
    Printf.printf "replace_child(%s, %s, %s)\n" (print_element parent) (print_element o) (print_element n);
  Element.insert_before parent o n

let rec sync : type old_msg msg. msg ctx -> Element.t -> bool ref -> Element.t ref -> old_msg ctrl -> msg vdom -> msg ctrl =
  fun ctx parent prev_move next old vdom ->
  let fallback () =
    let x = blit ~parent ctx vdom in
    let rec loop l1 l2 =
      match l1, l2 with
      | h1::t1, h2::t2 ->
          replace_child parent h1 h2;
          loop t1 t2
      | [], l ->
          List.iter (remove_child parent) l
      | l, [] ->
          List.iter (fun c -> insert_before parent c !next) l
    in
    loop (get_doms x) (get_doms old);
    dispose old;
    x
  in
  match old, vdom with
  | _ when (vdom : msg vdom) == (Obj.magic (get_vdom old : old_msg vdom)) ->
      (* If old and new vdom are physically identical, it is safe to reuse the old ctrl. *)
      (Obj.magic (old : old_msg ctrl) : msg ctrl)
  | BText {vdom = Text {txt = s1; key = _}; dom}, Text {txt = s2; key = _} ->
      if s1 <> s2 then Element.set_node_value dom s2;
      BText {vdom; dom}

  | BMap {child = c1; _}, Map {f; child = c2; key = _} ->
      let process msg = ctx.process (f msg) in
      let child = sync { ctx with process } parent prev_move next c1 c2 in
      BMap {vdom; doms = get_doms child; child; f}

  | BComponent {model_type = t1; state; child = c1; _}, Component { model_type = t2; view; update; _ } ->
      begin match same_type t1 t2 with
      | None -> fallback ()
      | Some Refl ->
          let process = process_component ctx parent state update in
          let c2 = view !state in
          let child = sync { ctx with process } parent prev_move next c1 c2 in
          BComponent {model_type = t1; vdom; doms = get_doms child; state; process; child}
      end

  | BGetContext {child = c1; _}, GetContext {context; child = c2; _} ->
    let arg = peek_context ctx context in
    let child = sync ctx parent prev_move next c1 (c2 arg) in
    BGetContext {vdom; doms = get_doms child; child}

  | BSetContext {child = c1; _}, SetContext {context; value; child = c2; _} ->
    let ctx = push_context ctx context value in
    let child = sync ctx parent prev_move next c1 c2 in
    BSetContext {vdom; doms = get_doms child; child}

  | BMemo {child = c1; vdom = Memo {f = f1; arg = a1; key = _}; _}, Memo {f = f2; arg = a2; key = _} ->
      (* Is this safe !? *)
      if Obj.magic f1 == f2 && Obj.magic a1 == a2 then
        bmemo vdom (Obj.magic (c1 : old_msg ctrl) : msg ctrl)
      else
        bmemo vdom (sync ctx parent prev_move next c1 (f2 a2))

  | BCustom {vdom = Custom {key=key1; elt=arg1; attributes=a1}; elt}, Custom {key=key2; elt=arg2; attributes=a2}
    when key1 = key2 && (arg1 == arg2 || elt.sync arg2) ->
      sync_attributes elt.dom a1 a2;
      BCustom {vdom; elt}

  | BFragment {vdom = Fragment e1; children; _}, Fragment e2 when e1.key = e2.key ->
      let children = sync_children ctx parent prev_move next children e2.children in
      let doms = List.concat_map get_doms children in
      BFragment {vdom; doms; children }

  | BElement {vdom = Element e1; dom; children}, Element e2 when e1.tag = e2.tag && e1.ns = e2.ns && e1.key = e2.key ->
      let children = sync_children ctx dom (ref false) (ref Element.null) children e2.children in
      (* synchronize properties & styles *)
      sync_attributes dom e1.attributes e2.attributes;
      BElement {vdom; dom; children}

  | _ -> fallback ()

and sync_children : type old_msg msg. msg ctx -> Element.t -> bool ref -> Element.t ref -> old_msg ctrl list -> msg vdom list -> msg ctrl list =
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
        sync ctx dom prev_move next old_children.(idx) new_children.(i)
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

type find =
  | NotFound
  | Found: {process: 'msg -> unit; mapper: ('inner_msg -> 'msg); inner: 'inner_msg ctrl; parent: find} -> find

let rec found: type inner_msg msg. (msg -> unit) -> (inner_msg -> msg) -> find -> Element.t -> inner_msg ctrl -> find =
  fun (type inner_msg msg) (process: msg -> unit) (mapper : inner_msg -> msg) parent dom -> function
    | BElement _ | BText _ | BCustom _ as inner -> Found {process; mapper; inner; parent}
    | BFragment {children; _} ->
        begin match List.find (fun c -> List.memq dom (get_doms c)) children with
        | exception Not_found -> assert false
        | c -> (found process mapper parent dom c : find)
        end
    | BComponent {child; process; _} -> found process Fun.id parent dom child
    | BMap {f; child; _} -> found process (fun x -> mapper (f x)) parent dom child
    | BMemo {child; _}
    | BSetContext {child; _}
    | BGetContext {child; _}  -> found process mapper parent dom child

(* Find a ctrl associated to a DOM element.
   Normalize by traversing Map node, and also return the composition of all such mappers
   from the root to the ctrl. *)

let rec vdom_of_dom: ('msg -> unit) -> 'msg ctrl -> Element.t -> find = fun process root dom ->
  (* hack to check dom == null?   Should move that to Ojs. *)
  match Ojs.option_of_js Element.t_of_js (Element.t_to_js dom) with
  | None -> NotFound
  | Some dom when List.memq dom (get_doms root) ->
      found process Fun.id NotFound dom root
  | Some dom ->
      begin match vdom_of_dom process root (Element.parent_node dom) with
      | NotFound -> NotFound
      | Found {process; mapper; inner = BElement {children; _}; _} as parent ->
          begin match List.find (fun c -> List.memq dom (get_doms c)) children with
          | exception Not_found -> assert false
          | c -> found process mapper parent dom c
          end
      | Found {mapper = _; inner = BCustom _; _} ->
          NotFound
      | _ -> assert false
      end

let mouse_event dom e =
  let client_x = Event.client_x e in
  let client_y = Event.client_y e in
  let brect = Element.get_bounding_client_rect dom in
  {
    x = client_x;
    y = client_y;
    page_x = Event.page_x e;
    page_y = Event.page_y e;
    element_x = (float client_x) -. Rect.left brect;
    element_y = (float client_y) -. Rect.top brect;
    buttons = Event.buttons e;
    alt_key = Event.alt_key e;
    ctrl_key = Event.ctrl_key e;
    shift_key = Event.shift_key e;
  }

let key_event e =
  {
    which = Event.which e;
    alt_key = Event.alt_key e;
    ctrl_key = Event.ctrl_key e;
    shift_key = Event.shift_key e;
  }

let paste_event dom e =
  {
    text = DataTransfer.get_data (Event.clipboard_data e) "text";
    selection_start = Element.selection_start dom;
    selection_end = Element.selection_end dom;
  }

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
    let l = List.rev !post_redraw  in
    post_redraw := [];
    List.iter (fun f -> f ()) l
  in

  let process_custom_fwd = ref (fun _ _ -> assert false) in
  let process_fwd = ref (fun _ -> assert false) in
  let redraw_fwd = ref (fun () -> assert false) in
  let run_cmd process element = Cmd.run after_redraw (env.cmds @ (!global).cmds) process element  in
  let ctx =
    {
      process_custom = (fun elt evt -> !process_custom_fwd elt evt);
      custom_handlers = env.customs;
      redraw = (fun () -> !redraw_fwd ());
      after_redraw;
      process = (fun msg -> !process_fwd msg);
      run_cmd;
      contexts = IntMap.empty
    }
  in
  let view model =
    try view model
    with exn ->
      Printf.printf "Error during vdom view: %s\n%!" (Printexc.to_string exn);
      raise exn
  in
  let x = blit ~parent:container ctx (view model0) in
  Window.request_animation_frame window flush;

  let model = ref model0 in
  let current = ref (Some x) in

  let pending_redraw = ref false in
  let redraw _ =
    (* TODO:
       could avoid calling view/sync if model is the same as the previous one
       (because updates are now batched
    *)
    match !current with
    | None -> ()
    | Some root ->
        pending_redraw := false;
        let x = sync ctx container (ref false) (ref Element.null) root (view !model) in
        current := Some x;
        flush ()
  in
  redraw_fwd := (fun () ->
    if not !pending_redraw then begin
      pending_redraw := true;
      Window.request_animation_frame window redraw
    end
  );
  let rec process msg =
    try
      let (new_model : model), (cmd : msg Vdom.Cmd.t) = update !model msg in
      model := new_model;
      run_cmd container cmd;
      if not !pending_redraw then begin
        pending_redraw := true;
        Window.request_animation_frame window redraw
      end
    with exn  ->
      Printf.printf "Error during vdom process: %s\n%!" (Printexc.to_string exn);
      raise exn
  and run_cmd (parent : Js_browser.Element.t) cmd =
    Cmd.run after_redraw (env.cmds @ (!global).cmds) process parent cmd
  in
  process_fwd := process;
  List.iter (Element.append_child container) (get_doms x);

  let prev_value_attribute = "data-ocaml-vdom-prev-value" in

  let onevent evt =
    let ty = Event.type_ evt in
    try
      let tgt = Element.t_of_js (Event.target evt) in
      let rec apply_handler dom = function
        | [] -> None
        | hd :: tl ->
            let may_cancel msg =
              match msg with
              | None -> None
              | Some _ as r -> Event.prevent_default evt; r
            in
            let res =
              match ty, hd with
              | "input", Handler (Input f) -> Some (f (Element.value tgt))
              | "blur", Handler (Change f) ->
                  (* cross browser emulation of change.
                     We remember the value when the field was last focused.  This does not work very well, since
                     the value could have changed since then because of a programmatic action (and is the "focus"
                     event even raised if the element is focused programmatically?). *)
                  let curr_value = Element.value tgt in
                  let changed =
                    not (Element.has_attribute tgt prev_value_attribute) ||
                    Element.get_attribute tgt prev_value_attribute <> curr_value
                  in
                  if changed then Some (f curr_value) else None
              | "change", Handler (ChangeIndex f) -> Some (f (Element.selected_index tgt))
              | "click", Handler (ChangeChecked f) -> Some (f (Element.checked tgt))
              | "click", Handler (Click f) -> Some (f (mouse_event dom evt))
              | "click", Handler (ClickCancel f) -> may_cancel (f (mouse_event dom evt))
              | "dblclick", Handler (DblClick f) -> Some (f (mouse_event dom evt))
              | "blur", Handler (Blur msg) -> Some msg
              | "focus", Handler (Focus msg) -> Some msg
              | "mousemove", Handler (MouseMove f) -> Some (f (mouse_event dom evt))
              | "mousedown", Handler (MouseDown f) -> Some (f (mouse_event dom evt))
              | "mousedown", Handler (MouseDownCancel f) -> may_cancel (f (mouse_event dom evt))
              | "mouseup", Handler (MouseUp f) -> Some (f (mouse_event dom evt))
              | "mouseenter", Handler (MouseEnter f) when tgt = dom -> Some (f (mouse_event dom evt))
              | "mouseleave", Handler (MouseLeave f) when tgt = dom -> Some (f (mouse_event dom evt))
              | "mouseover", Handler (MouseOver f) -> Some (f (mouse_event dom evt))
              | "keydown", Handler (KeyDown f) -> Some (f (key_event evt))
              | "keydown", Handler (KeyDownCancel f) -> may_cancel (f (key_event evt))
              | "keyup", Handler (KeyUp f) -> Some (f (key_event evt))
              | "keyup", Handler (KeyUpCancel f) -> may_cancel (f (key_event evt))
              | "contextmenu", Handler (ContextMenu f) -> Event.prevent_default evt; Some (f (mouse_event dom evt))
              | "paste", Handler (Paste f) -> may_cancel (f (paste_event dom evt))
              | _ -> None
            in
            match res with
            | Some _ -> res
            | None -> apply_handler dom tl
      in
      let rec propagate = function
        | Found {
            process;
            mapper;
            inner = ( BElement {vdom = Element {attributes; _}; dom; _}
                    | BCustom  {vdom = Custom  {attributes; _}; elt = {dom; _}; _} );
            parent;
          } ->
            (* see "cross browser emulation change" comment above *)
            if ty = "focus" && List.exists (function Handler (Change _) -> true | _ -> false) attributes then
              Element.set_attribute tgt prev_value_attribute (Element.value tgt);
            begin match apply_handler dom attributes with
            | None -> propagate parent
            | Some msg -> process (mapper msg)
            end
        | _ ->
            ()
      in

      Option.iter (fun root ->
          propagate (vdom_of_dom process root tgt);
        ) !current;

      if ty = "input" || ty = "blur" then
        let f () =
          Option.iter
            (fun root ->
               match vdom_of_dom process root tgt with
               (* note: the new vdom can be different after processing
                  the event above *)
               (* !! This is probably broken now that we delay updating the vdom
                     with request_animation_frame !! *)
               | Found {process = _ ; mapper = _; inner = BElement {vdom = Element {attributes; _}; _}; _} ->
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

  let process_custom tgt event =
    Option.iter
      (fun root ->
         let apply process mapper attributes =
           let select_handler = function
             | Handler h -> event.ev h
             | _ -> None
           in
           let msgs = List.filter_map select_handler attributes in
           List.iter (fun msg -> process (mapper msg)) msgs
         in
         begin match vdom_of_dom process root tgt with
         | Found {process; mapper; inner = BElement {vdom = Element {attributes; _}; _}; _} ->
             apply process mapper attributes
         | Found {process; mapper; inner = BCustom  {vdom = Custom  {attributes; _}; _}; _} ->
             apply process mapper attributes
         | _ ->
             ()
         end
      ) !current
      (* Do we need to do something similar to the "input" case in onevent? *)
  in
  process_custom_fwd := process_custom;

  let listeners =
    List.map
      (fun (event, capture) ->
         Element.add_cancellable_event_listener container event onevent capture
      )
      [
        Event.Click, false;
        Event.Dblclick, false;
        Event.Input, false;
        Event.Change, false;
        Event.Focus, true;
        Event.Blur, true;
        Event.Mousemove, true;
        Event.Mouseenter, true;
        Event.Mouseleave, true;
        Event.Mouseover, true;
        Event.Mousedown, true;
        Event.Mouseup, true;
        Event.Keydown, true;
        Event.Contextmenu, true;
        Event.Paste, false; (* ? *)
      ]
  in
  run_cmd container cmd0;
  let dispose () =
    Option.iter
      (fun root ->
         current := None;
         dispose root;
         List.iter (fun f -> f ()) listeners;
         if container_created then
           Element.remove container
         else
           Element.set_inner_HTML container ""
      ) !current
  in
  {dom = container; process; get = (fun () -> !model); after_redraw; dispose}

let dispose {dispose; _} = dispose ()
