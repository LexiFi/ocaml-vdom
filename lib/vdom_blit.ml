(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)


open Js_browser
open Vdom

let debug = false

module Cmd = struct
  type 'msg ctx =
    {
      send_msg: ('msg -> unit);
    }

  let send_msg ctx = ctx.send_msg

  type handler = {f: 'msg. 'msg ctx -> 'msg Vdom.Cmd.t -> bool}

  let rec run: type t. handler list -> (t -> unit) -> t Cmd.t -> unit = fun h p -> function
    | Cmd.Echo msg -> p msg
    | Cmd.Batch l -> List.iter (run h p) l
    | Cmd.Map (f, cmd) -> run h (fun x -> p (f x)) cmd
    | x ->
        let ctx = {send_msg = p} in
        let rec loop = function
          | [] -> failwith (Printf.sprintf "No command handler found! (%s)" (Obj.Extension_constructor.name (Obj.Extension_constructor.of_val x)))
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

(* Auto scrolling *)

let rec scroll_parent node =
  if node = Element.null then
    Document.body document
  else
    let overflow_y = Style.get (Window.get_computed_style window node) "overflowY" in
    let is_scrollable = overflow_y <> "visible" && overflow_y <> "hidden" in
    if is_scrollable && Element.scroll_height node >= Element.client_height node then
      node
    else
      scroll_parent (Element.parent_node node)

let scroll_to_make_visible child =
  let open Js_browser in
  let parent = scroll_parent child in
  let r_parent = Element.get_bounding_client_rect parent in
  let r_child = Element.get_bounding_client_rect child in
  let y1 = Rect.bottom r_parent and y2 = Rect.bottom r_child in
  if y2 > y1 then
    Element.set_scroll_top parent (Element.scroll_top parent +. y2 -. y1)
  else
    let y1 = Rect.top r_parent and y2 = Rect.top r_child in
    if y2 < y1 then
      Element.set_scroll_top parent (Element.scroll_top parent +. y2 -. y1)

(* Rendering (VDOM -> DOM) *)


type 'msg ctrl =
  | BText of {vdom: 'msg vdom; dom: Element.t}
  | BElement of {vdom: 'msg vdom; dom: Element.t; children: 'msg ctrl list}
  | BMap: {vdom: 'msg vdom; dom: Element.t; f: ('submsg -> 'msg); child: 'submsg ctrl} -> 'msg ctrl
  | BMemo: {vdom: 'msg vdom; dom: Element.t; child: 'msg ctrl} -> 'msg ctrl
  | BCustom of {vdom: 'msg vdom; elt: Custom.t}

let get_dom = function
  | BText x -> x.dom
  | BElement x -> x.dom
  | BMap x -> x.dom
  | BMemo x -> x.dom
  | BCustom x -> x.elt.dom

let get_vdom = function
  | BText x -> x.vdom
  | BElement x -> x.vdom
  | BMap x -> x.vdom
  | BMemo x -> x.vdom
  | BCustom x -> x.vdom

let key_of_vdom = function
  | Text {key; _}
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
  BMemo {vdom; dom = get_dom child; child}

let async f =
  ignore (Window.set_timeout window f 0)

let custom_attribute dom = function
  | "scroll-to-show" ->
      async
        (fun () ->
           try scroll_to_make_visible dom
           with exn -> Printf.printf "scroll: %s\n%!" (Printexc.to_string exn)
        );
      true

  | "autofocus" ->
      async (fun () -> Element.focus dom);
      true

  | "relative-dropdown" ->
      let style = Element.style dom in
      Style.set_position (Element.style dom) "absolute";
      let px = Printf.sprintf "%fpx" in
      async (fun () ->
          match Element.offset_parent dom with
          | None -> ()
          | Some offset_parent ->
              let parent = Element.parent_node dom in
              let rect = Element.get_bounding_client_rect parent in
              let offset_rect = Element.get_bounding_client_rect offset_parent in
              let top = Rect.top rect -. Rect.top offset_rect in
              let left = Rect.left rect -. Rect.left offset_rect in
              Style.set_top style (px (top +. float (Element.offset_height parent)));
              Style.set_left style (px left);
              Style.set_width style (px (float (Element.offset_width parent)))
        );
      true

  | _ -> false



let apply_attributes dom attributes =
  List.iter
    (function
      | Property (k, v) ->
          if not (custom_attribute dom k) then
            Ojs.set (Element.t_to_js dom) k (eval_prop v)

      | Style (k, v) ->
          Ojs.set
            (Ojs.get (Element.t_to_js dom) "style")
            k
            (Ojs.string_to_js v)

      | Attribute (k, v) ->
          Element.set_attribute dom k v

      | _ -> ()
    )
    attributes

type ctx =
  {
    process_custom: (Element.t -> event -> unit);
    custom_handlers: Custom.handler list;
    after_redraw: ((unit -> unit) -> unit);
  }

let rec blit : 'msg. parent:_ -> ctx -> 'msg vdom -> 'msg ctrl =
  fun ~parent ctx vdom ->
  match vdom with
  | Text {txt; key = _} ->
      BText {vdom; dom = Document.create_text_node document txt}

  | Map {f; child; key = _} ->
      let child = blit ~parent ctx child in
      BMap {vdom; dom = get_dom child; f; child}

  | Memo {f; arg; key = _} ->
      bmemo vdom (blit ~parent ctx (f arg))

  | Custom {elt; attributes; key = _} ->
      let elt =
        try Custom.lookup ~parent ~process_custom:ctx.process_custom ~after_redraw:ctx.after_redraw elt ctx.custom_handlers
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
      List.iter (fun c -> Element.append_child dom (get_dom c)) children;
      apply_attributes dom attributes;
      BElement {vdom; dom; children}

let blit ctx vdom =
  try blit ctx vdom
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
        if not (custom_attribute dom k) then
          Ojs.set (Element.t_to_js dom) k (eval_prop v)
  in
  let clear k = Ojs.set (Element.t_to_js dom) k Ojs.null in
  sync_props
    string_of_prop
    same_prop
    set clear
    (choose props a1)
    (choose props a2);

  let styles = function Style (k, v) -> Some (k, String v) | Property _ | Handler _ | Attribute _ -> None in
  let set k v = Ojs.set (Ojs.get (Element.t_to_js dom) "style") k (eval_prop v) in
  let clear k = Ojs.set (Ojs.get (Element.t_to_js dom) "style") k js_empty_string in
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
  | BElement {children; _} -> List.iter dispose children
  | BMap {child; _} -> dispose child
  | BMemo {child; _} -> dispose child

let rec sync : type old_msg msg. ctx -> Element.t -> old_msg ctrl -> msg vdom -> msg ctrl =
  fun ctx parent old vdom ->

  match old, vdom with
  | _ when (vdom : msg vdom) == (Obj.magic (get_vdom old : old_msg vdom)) ->
      (* If old and new vdom are physically identical, it is safe to reuse the old ctrl. *)
      (Obj.magic (old : old_msg ctrl) : msg ctrl)
  | BText {vdom = Text {txt = s1; key = _}; dom}, Text {txt = s2; key = _} ->
      if s1 <> s2 then Element.set_node_value dom s2;
      BText {vdom; dom}

  | BMap {child = c1; _}, Map {f; child = c2; key = _} ->
      let child = sync ctx parent c1 c2 in
      BMap {vdom; dom = get_dom child; child; f}

  | BMemo {child = c1; vdom = Memo {f = f1; arg = a1; key = _}; _}, Memo {f = f2; arg = a2; key = _} ->
      (* Is this safe !? *)
      if Obj.magic f1 == f2 && Obj.magic a1 == a2 then
        bmemo vdom (Obj.magic (c1 : old_msg ctrl) : msg ctrl)
      else
        bmemo vdom (sync ctx parent c1 (f2 a2))

  | BCustom {vdom = Custom {key=key1; elt=arg1; attributes=a1}; elt}, Custom {key=key2; elt=arg2; attributes=a2}
    when key1 = key2 && (arg1 == arg2 || elt.sync arg2) ->
      sync_attributes elt.dom a1 a2;
      BCustom {vdom; elt}

  | BElement {vdom = Element e1; dom; children}, Element e2 when e1.tag = e2.tag && e1.ns = e2.ns && e1.key = e2.key ->

      (* TODO:
         - add a fast-path to deal with prefixes and suffixes of old/new children with identical
           keys, avoiding a lot of allocations.
         - use a JS object (map) instead of an OCaml Hashtbl?
      *)

      (* synchronize children *)

      let old_children = Array.of_list children in
      let new_children = Array.of_list e2.children in

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
           Element.remove_child dom (get_dom to_remove);
           dispose to_remove
        )
        by_key;

      (* produce the new sequence, from right-to-left, creating and picking+syncinc nodes *)
      let ctrls = ref [] in
      let next = ref (Element.t_of_js Ojs.null) in
      for i = Array.length new_children - 1 downto 0 do
        let idx = indices.(i) in
        if debug then Printf.printf "old = %i; new = %i: " idx i;
        let c =
          if idx < 0 then begin
            (* create *)
            if debug then Printf.printf "create\n%!";
            blit ~parent ctx new_children.(i)
          end
          else begin
            if debug then Printf.printf "sync&move\n%!";
            (* note: the sync could lead to a DOM replace,
               following by a move below; in that case,
               one should just delete old + insert new *)
            sync ctx dom old_children.(idx) new_children.(i)
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

        let move =
          idx < 0 ||
          ((if i = Array.length new_children - 1 then idx <> Array.length old_children - 1
            else indices.(i + 1) <> idx + 1)
           && Element.next_sibling (get_dom c) != !next) (* could avoid reading from the DOM... *)
        in
        if move then begin
          if debug then Printf.printf "really move\n%!";
          Element.insert_before dom (get_dom c) !next;
        end;
        next := get_dom c;
        ctrls := c :: !ctrls
      done;

      let children = !ctrls in

      (* synchronize properties & styles *)
      sync_attributes dom e1.attributes e2.attributes;
      BElement {vdom; dom; children}

  | _ ->
      let x = blit ~parent ctx vdom in
      Element.replace_child parent (get_dom x) (get_dom old);
      dispose old;
      x

let sync ctx parent old vdom =
  try sync ctx parent old vdom
  with exn ->
    Printf.printf "Error during vdom sync: %s\n%!" (Printexc.to_string exn);
    raise exn

type 'msg find =
  | NotFound
  | Found: {mapper: ('inner_msg -> 'msg); inner: 'inner_msg ctrl; parent: 'msg find} -> 'msg find

let rec found: type inner_msg msg. (inner_msg -> msg) -> msg find -> inner_msg ctrl -> msg find = fun mapper parent -> function
  | BElement _ | BText _ | BCustom _ as inner -> Found {mapper; inner; parent}
  | BMap {f; child; _} -> found (fun x -> mapper (f x)) parent child
  | BMemo {child; _} -> found mapper parent child

(* Find a ctrl associated to a DOM element.
   Normalize by traversing Map node, and also return the composition of all such mappers
   from the root to the ctrl. *)

let rec vdom_of_dom: type msg. msg ctrl -> Element.t -> msg find = fun root dom ->
  (* hack to check dom == null?   Should move that to Ojs. *)
  match Ojs.option_of_js Element.t_of_js (Element.t_to_js dom) with
  | None -> NotFound
  | Some dom when dom == get_dom root ->
      found Fun.id NotFound root
  | Some dom ->
      begin match vdom_of_dom root (Element.parent_node dom) with
      | NotFound -> NotFound
      | Found {mapper; inner = BElement {children; _}; _} as parent ->
          begin match List.find (fun c -> get_dom c == dom) children with
          | exception Not_found -> assert false
          | c -> found mapper parent c
          end
      | Found {mapper = _; inner = BCustom _; _} ->
          NotFound
      | _ -> assert false
      end

let mouse_event e =
  {
    x = Event.client_x e;
    y = Event.client_y e;
    page_x = Event.page_x e;
    page_y = Event.page_y e;
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
  let ctx =
    {
      process_custom = (fun elt evt -> !process_custom_fwd elt evt);
      custom_handlers = env.customs;
      after_redraw;
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
        let x = sync ctx container root (view !model) in
        current := Some x;
        flush ()
  in

  let rec process msg =
    try
      let (new_model : model), (cmd : msg Vdom.Cmd.t) = update !model msg in
      model := new_model;
      Cmd.run env.cmds process cmd;
      if not !pending_redraw then begin
        pending_redraw := true;
        Window.request_animation_frame window redraw
      end
    with exn  ->
      Printf.printf "Error during vdom process: %s\n%!" (Printexc.to_string exn);
      raise exn
  in

  Element.append_child container (get_dom x);

  let prev_value_attribute = "data-ocaml-vdom-prev-value" in

  let onevent evt =
    let ty = Event.type_ evt in
    try
      let tgt = Element.t_of_js (Event.target evt) in
      let rec apply_handler = function
        | [] -> None
        | hd :: tl ->
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
              | "click", Handler (Click f) -> Some (f (mouse_event evt))
              | "dblclick", Handler (DblClick f) -> Some (f (mouse_event evt))
              | "blur", Handler (Blur msg) -> Some msg
              | "focus", Handler (Focus msg) -> Some msg
              | "mousemove", Handler (MouseMove f) -> Some (f (mouse_event evt))
              | "mousedown", Handler (MouseDown f) -> Some (f (mouse_event evt))
              | "keydown", Handler (KeyDown f) -> Some (f (key_event evt))
              | "keydown", Handler (KeyDownCancel f) ->
                  begin match f (key_event evt) with
                  | None -> None
                  | Some _ as r -> Event.prevent_default evt; r
                  end
              | "contextmenu", Handler (ContextMenu f) -> Event.prevent_default evt; Some (f (mouse_event evt))
              | _ -> None
            in
            match res with
            | Some _ -> res
            | None -> apply_handler tl
      in
      let rec propagate = function
        | Found {
            mapper;
            inner = ( BElement {vdom = Element {attributes; _}; _}
                    | BCustom  {vdom = Custom  {attributes; _}; _} );
            parent;
          } ->
            (* see "cross browser emulation change" comment above *)
            if ty = "focus" && List.exists (function Handler (Change _) -> true | _ -> false) attributes then
              Element.set_attribute tgt prev_value_attribute (Element.value tgt);
            begin match apply_handler attributes with
            | None -> propagate parent
            | Some msg -> process (mapper msg)
            end
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

  let process_custom tgt event =
    Option.iter
      (fun root ->
         begin match vdom_of_dom root tgt with
         | Found {mapper; inner = BCustom  {vdom = Custom  {attributes; _}; _}; _} ->
             let select_handler = function
               | Handler h -> event.ev h
               | _ -> None
             in
             let msgs = List.filter_map select_handler attributes in
             List.iter (fun msg -> process (mapper msg)) msgs
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
        Event.Mousedown, true;
        Event.Keydown, true;
        Event.Contextmenu, true;
      ]
  in
  Cmd.run env.cmds process cmd0;
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