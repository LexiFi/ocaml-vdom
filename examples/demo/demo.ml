(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

open Js_browser
open Vdom

(* Custom commands *)

type 'msg Vdom.Cmd.t +=
  | Http_get of {url: string; payload: string; on_success: (string -> 'msg)}
  | After of int * 'msg

let http_get (type msg) ~url ~payload (on_success : _ -> msg) : msg Vdom.Cmd.t =
  Http_get {url; payload; on_success}

let after x f = After (x, f)


let button ?(a = []) txt f = input [] ~a:(onclick (fun _ -> f) :: type_button :: value txt :: a)
let br = elt "br" []


module Demo1 = struct
  type model =
    {
      i: int;
      s: string;
    }

  let view {i; s} =
    div
      [
        txt_span
          ~a:[style "font-size" (Printf.sprintf "%ipx" (i*2))]
          (string_of_int i);

        button "+" ~a:[disabled (i >= 10)] `Plus;
        (if i = 0 then txt_span "" else button "-" (`Set (i - 1)));
        br;
        text (string_of_int i);
        br;
        input []
          ~a:[
            oninput (fun s ->
                match int_of_string s with
                | exception _ -> if s <> "" then `Text "ERR" else `Noop
                | i -> `Set i
              );
            value (string_of_int i)
          ];
        br;

        input [] ~a:[oninput (fun s -> `Text s); value s];

        map (function `Text s -> `Text (s ^ "+") | r -> r)
          (input [] ~a:[oninput (fun s -> `Text s); value s]);

        input [] ~a:[value "FIXED"];

        text s;
      ]

  let init = return {i = 5; s = "Hello"}

  let update model = function
    | `Plus -> return {model with i = model.i + 1}
    | `Set i -> return {model with i}
    | `Text s -> return {model with s}
    | `Noop -> return model

  let app = {init; update; view}
end


module Demo2 = struct
  type model =
    {
      n: int;
    }

  let init = return { n = 5 }

  let update model = function
    | `Set s ->
        begin match int_of_string s with
        | exception _ -> return model
        | n when n >= 0 -> return {n}
        | _ -> return model
        end
    | `Click x -> Printf.printf "clicked %i\n" x; return model

  let view { n } =
    let rec loop acc i =
      if i = 0 then acc
      else loop (button (string_of_int i) (`Click i) :: acc) (i - 1)
    in
    let input = input ~key:"foo" [] ~a:[oninput (fun s -> `Set s)] in
    let buttons = loop [] n in
    let l = if n mod 2 = 0 then input :: buttons else buttons @ [input] in
    div l

  let app = {init; update; view}
end

module Demo3 = struct

  type foo = { x: int; y: int }
  type bar = { a: foo; b: foo }

  let init =
    return { a = {x=10; y=10}; b = {x=20; y=20} }
      ~c:[after 1000 `Inc; after 2000 `Inc]

  let update model = function
    | `Swap2 -> return { a = model.b; b = model.a }
    | `Swap1 -> return { a = { x = model.a.y; y = model.a.x }; b = model.b }
    | `Inc -> return { a = { x = model.a.x + 1; y = model.a.y }; b = model.b }
    | `DelayedReset -> return model ~c:[after 1000 `Reset]
    | `Reset -> init

  let show {x; y} =
    Printf.printf "show %i/%i\n%!" x y;
    elt "p" [ text (Printf.sprintf "%i/%i" x y) ]


  let view model =
    div
      [
        button "A<->B" `Swap2;
        button "A.x<->A.y" `Swap1;
        button "A.x++" `Inc;
        button "Delayed reset" `DelayedReset;
        memo show model.a;
        memo show model.b;
      ]

  let app = {init; update; view}
end

module DemoHttp = struct

  type model =
    {
      url: string;
      focused: bool;
      content: [`Nothing|`Loading of string|`Data of string];
    }

  let init =
    return
      {
        url = "";
        focused = false;
        content = `Nothing;
      }

  let update model = function
    | `Set url ->
        return {model with url}
    | `FetchStart ->
        return {model with content = `Loading model.url}
          ~c:[after 2000 (`Fetch model.url)]
    | `Fetch url ->
        return model
          ~c:[http_get ~url ~payload:"" (fun r -> `Fetched r)]
    | `Fetched s ->
        return {model with content = `Data s}
    | `Focused b ->
        return {model with focused = b}

  let view {url; focused; content} =
    div
      [
        input
          ~a:[
            int_prop "size" (if focused then 200 else 100);
            value url;
            oninput (fun s -> `Set s);
            onfocus (`Focused true);
            onblur (`Focused false)
          ]
          [];
        div [button "Fetch" `FetchStart];
        begin match content with
        | `Nothing -> text "Please type an URL to load."
        | `Loading url -> text (Printf.sprintf "Loading %s, please wait..." url)
        | `Data data -> elt "pre" [ text data ]
        end
      ]

  let app = {init; update; view}
end


module Pair = struct
  type ('a, 'b) msg = Left of 'a | Right of 'b
  let left x = Left x
  let right x = Right x

  let app app1 app2 =
    let init =
      return
        (fst app1.init, fst app2.init)
        ~c:[Cmd.map left (snd app1.init); Cmd.map right (snd app2.init)]
    in
    let view (model1, model2) =
      div
        [
          map left (memo app1.view model1);
          map right (memo app2.view model2);
        ]
    in
    let update (model1, model2) = function
      | Left x ->
          let (model1, cmd) = app1.update model1 x in
          return (model1, model2) ~c:[Cmd.map left cmd]
      | Right x ->
          let (model2, cmd) = app2.update model2 x in
          return (model1, model2) ~c:[Cmd.map right cmd]
    in
    {init; update; view}
end

module MouseMove = struct

  type evt = {x: int; y: int; buttons: int}

  let init = return {x = 0; y = 0; buttons = 0}

  let view ({x; y; buttons} : evt) =
    elt "span"
      ~a:[
        onmousemove (fun {Vdom.x; y; buttons; _} -> {x; y; buttons});
        style "background-color" "red"
      ]
      [
        text (Printf.sprintf "x = %i; y = %i; buttons = %i" x y buttons)
      ]

  let update _ evt = return evt

  let app = {init; update; view}
end

module Talk1 = struct
  type model = { x: int; y: int }
  type msg = Inc

  let button txt msg =
    elt "input" []
      ~a:[onclick (fun _ -> msg); str_prop "type" "button";
          str_prop "value" txt;
         ]

  let view {x; y} =
    elt "div" [
      button "Increment" Inc;
      text (Printf.sprintf "x=%d / y=%d" x y)
    ]

  let init = { x = 0; y = 0 }

  let update model = function
    | Inc -> {model with x = model.x + 1}

  let app = simple_app ~init ~update ~view ()
end


module Talk2 = struct
  type model = { s: string }
  type msg = Set of string
  let set s = Set s

  let view {s} =
    let v = elt "input" ~a:[str_prop "value" s; oninput set] [] in
    elt "div" [v; v]

  let init = {s = ""}

  let update _model = function
    | Set s -> {s}

  let app = simple_app ~init ~update ~view ()
end


module DemoSelection = struct

  open Vdom_ui

  type model =
    {
      s: string;
      select: int SelectionList.model;
    }

  let init =
    let rec gen = function 100 -> [] | i -> i :: gen (i + 1) in
    { s = ""; select = SelectionList.init (gen 0) }

  let view {s; select} =
    elt "div"
      [
        text s;
        SelectionList.view string_of_int (fun x -> `Internal x) (fun x -> `Select x) select;
      ]

  let update state = function
    | `Internal x -> {state with select = SelectionList.update state.select x}
    | `Select x -> {state with s = Printf.sprintf "Selection : %i" x}

  let app = simple_app ~init ~update ~view ()
end

module DemoCheckbox = struct
  type model =
    {
      checked: bool;
    }

  let init = {checked = true}

  let view {checked} =
    div
      [
        elt "input" []
          ~a:[str_prop "type" "checkbox";
              bool_prop "checked" checked;
              onclick (fun _ -> `Click1);
             ];
        elt "input" []
          ~a:[str_prop "type" "checkbox";
              bool_prop "checked" checked;
              onclick (fun _ -> `Click1);
             ];
        elt "input" []
          ~a:[str_prop "type" "checkbox";
              bool_prop "checked" (not checked);
              onclick (fun _ -> `Click1);
             ];
        elt "input" []
          ~a:[str_prop "type" "checkbox";
              bool_prop "checked" checked;
             ];
        elt "input" []
          ~a:[str_prop "type" "checkbox";
              bool_prop "checked" true;
             ];
        elt "input" []
          ~a:[str_prop "type" "checkbox";
              onchange_checked (fun b -> `Change b);
             ];
      ]

  let update model = function
    | `Click1 -> {checked=not model.checked}
    | `Change b -> {checked=b}

  let app = simple_app ~init ~update ~view ()
end


module Issue18_propagation = struct
  open Vdom

  type model = Unclicked | Clicked of string

  type message = Click of string | Reset

  let view =
    function
    | Unclicked ->
        div ~a:[onclick (fun _ -> Click "outer"); class_ "outer"]
          [
            div ~a:[ class_ "inner"] [text "inside the inner div"];
            div ~a:[ class_ "inner"; onclick (fun _ -> Click "inner")]
              [text "inner div with own click handler"];
            text "outside the inner div";
          ]
    | Clicked s ->
        div ~a:[onclick (fun _ -> Reset)] [text (Printf.sprintf "Clicked: %s" s)]

  let init = Unclicked

  let update _m = function
    | Click s -> Clicked s
    | Reset -> Unclicked

  let app = simple_app ~init ~view ~update ()

end

(* Custom command handlers *)

let run_http_get ~url ~payload ~on_success () =
  let open XHR in
  let r = create () in
  open_ r "GET" url;
  set_response_type r "text";
  set_onreadystatechange r
    (fun () ->
       match ready_state r with
       | Done -> on_success (response_text r)
       | _ ->
           ()
    );
  send r (Ojs.string_to_js payload)

let cmd_handler ctx = function
  | Http_get {url; payload; on_success} ->
      run_http_get ~url ~payload ~on_success:(fun s -> Vdom_blit.Cmd.send_msg ctx (on_success s)) ();
      true
  | After (n, msg) ->
      ignore (Window.set_timeout window (fun () -> Vdom_blit.Cmd.send_msg ctx msg) n);
      true
  | _ ->
      false

let () = Vdom_blit.(register (cmd {f = cmd_handler}))

let test_dispose = true

let run () =
  let body = Document.body document in
  let r app =
    let container = Document.create_element document "div" in
    let app = Vdom_blit.run ~container app in
    Element.append_child body container;
    if test_dispose then begin
      let button = Document.create_element document "button" in
      Element.append_child button (Document.create_text_node document "Dispose Application");
      Element.add_event_listener button Event.Click (fun _ ->
          Element.remove button;
          Vdom_blit.dispose app;
          Element.append_child container (Document.create_text_node document "Disposed");
        ) false;
      Element.append_child body button;
    end;
    Element.append_child body (Document.create_element document "hr");
  in

  r Talk1.app;
  r Talk2.app;

  r Demo1.app;
  r Demo1.app;
  r Demo2.app;
  r Demo3.app;
  r (Pair.app DemoHttp.app Demo3.app);
  r MouseMove.app;
  r DemoSelection.app;
  r DemoCheckbox.app;
  r Issue18_propagation.app;
  ()

let () = Window.set_onload window run
