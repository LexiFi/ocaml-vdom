(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2023 LexiFi                                                                    *)

(* Inspired from https://github.com/janestreet/incr_dom/blob/master/example/incr_decr/counters.ml *)

open Vdom

module IntMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type model = {
  counters : int IntMap.t;
}

let update { counters } = function
  | `New_counter -> { counters = IntMap.add (IntMap.cardinal counters) 0 counters }
  | `Update (pos, diff) -> { counters = IntMap.add pos (IntMap.find pos counters + diff) counters }

let init = { counters = IntMap.empty }

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let view { counters } =
  let row (pos, value) =
    div [button "-" (`Update (pos, -1)); text (string_of_int value); button "+" (`Update (pos, 1))]
  in
  div (div [button "New counter" `New_counter] :: (IntMap.bindings counters |> List.map row))


let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = Window.set_onload window run
