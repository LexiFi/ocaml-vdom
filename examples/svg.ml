(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

open Vdom

type model = int

let update n = function
  | `Click -> n mod 5 + 1

let init = 1

let view n =
  div
    [
      text (string_of_int n);
      svg_elt "svg"
        ~a:[
          int_attr "width" (n * 20);
          int_attr "height" (n * 20);
        ]
        [
          svg_elt "circle" []
            ~a:[
              onclick (fun _ -> `Click);
              int_attr "cx" (n * 10);
              int_attr "cy" (n * 10);
              int_attr "r" (n * 10);
              attr "fill" (if n mod 2 = 0 then "green" else "blue");
            ]
        ]
    ]

let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = Window.set_onload window run
