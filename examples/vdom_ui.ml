(***************************************************************************)
(*  Copyright (C) 2000-2016 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


open Js_browser

let contains ~pattern s =
  let rec loop i =
    if i < 0 then false
    else String.sub s i (String.length pattern) = pattern || loop (i - 1)
  in
  loop (String.length s - String.length pattern)

module SelectionList = struct
  open Vdom

  type 'a model =
    {
      list: 'a list;
      cursor: int;
      filter: string;
    }

  type msg =
    [`Cursor of int | `Filter of string | `Nop]

  let view show msg select {list; cursor; filter} =
    let keep = contains ~pattern:filter in
    let instrs = List.filter (fun x -> keep (show x)) list in
    let l =
      List.mapi
        (fun i x ->
           let a = if i = cursor then
               [style "background-color" "#E0E0E0";
                scroll_to_show] else [] in
           div
             ~a:(onclick (select x) :: class_ "link" :: a)
             [ text (show x) ]
        )
        instrs
    in
    div
      [
        elt "input" [] ~a:
          [type_ "search"; str_prop "placeholder" "Search...";
           class_ "searchbox"; value filter;
           autofocus;
           oninput (fun s -> msg (`Filter s));
           onkeydown
             (fun e ->
                match e.which with
                | 38 -> msg (`Cursor (max (cursor - 1) 0))
                | 40 -> msg (`Cursor (min (cursor + 1) (List.length instrs - 1)))
                | 33 -> msg (`Cursor (max (cursor - 15) 0))
                | 34 -> msg (`Cursor (min (cursor + 15) (List.length instrs - 1)))
                | 36 -> msg (`Cursor 0)
                | 35 -> msg (`Cursor (List.length instrs - 1))
                | 13 -> select (List.nth instrs cursor)
                | _ -> msg `Nop
             )
          ];
        div ~a:[style "width" "300px"; style "margin-top" "30px";
                style "overflow-y" "scroll";
                style "height" "500px";
               ] l
      ]

  let update model = function
    | `Filter filter -> {model with filter; cursor = 0}
    | `Cursor cursor -> {model with cursor}
    | `Nop -> model

  let init list = {list; filter = ""; cursor = 0}
end


module Initializable = struct
  type 'a model =
    | Initializing
    | Ready of 'a

  type ('a, 'msg) msg = [`Got of 'a | `Internal of 'msg]

  let internal msg = `Internal msg
  let got x = `Got x

  let update f model msg =
    match model, msg with
    | Initializing, `Got x -> Vdom.return (Ready x)
    | Ready model, `Internal msg -> let m, a = f model msg in Vdom.return (Ready m) ~c:[Vdom.Cmd.map internal a]
    | _ -> Vdom.return model

  let view f = function
    | Initializing -> Vdom.text "Please wait..."
    | Ready model -> Vdom.map internal (f model)

  let init f =
    Vdom.return Initializing ~c:[Vdom.Cmd.map got f]

  let app ~init:i ~view:v ~update:u () =
    Vdom.app ~init:(init i) ~update:(update u) ~view:(view v) ()
end
