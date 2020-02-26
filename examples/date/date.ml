(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

open Vdom

type model = float

let update _ = function
  | `Click -> Js_browser.Date.now ()

let init = 0.

let view n =
  let t = Js_browser.Date.new_date n in
  div
    [
      div [text (Printf.sprintf "protocol: %S" (Js_browser.Location.protocol (Js_browser.Window.location Js_browser.window)))];
      div [text (Printf.sprintf "Number of milliseconds: %f" n)];
      div [text (Printf.sprintf "ToDateString: %s" (Js_browser.Date.to_date_string t))];
      div [text (Printf.sprintf "ToLocaleString: %s" (Js_browser.Date.to_locale_string t))];
      div [input [] ~a:[onclick (fun _ -> `Click); type_button; value "Update"]]
    ]

let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = Window.set_onload window run
