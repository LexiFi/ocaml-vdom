type direction =
  | One_way
  | Return

type date =
  int * int * int

type model =
  {
    direction: direction;
    start_date: date;
    end_date: date;
  }

type msg =
  | Selected of direction
  | Start_date of string
  | End_date of string
  | Clicked

let init date =
  {
    direction = One_way;
    start_date = date;
    end_date = date;
  }

let string_of_date (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

let parse_date s =
  Scanf.sscanf s "%04d-%02d-%02d" (fun y m d -> (y, m, d))

let view {direction; start_date; end_date} =
  let select =
    let option d =
      let s = match d with One_way -> "one-way flight" | Return -> "return flight" in
      let a = if direction = d then [Vdom.attr "selected" ""] else [] in
      Vdom.elt "option" ~a [Vdom.text s]
    in
    let onchange = function
      | 0 -> Selected One_way
      | 1 -> Selected Return
      | _ -> assert false (* cannot happen *)
    in
    Vdom.elt "select" ~a:[Vdom.onchange_index onchange] [option One_way; option Return]
  in
  let date_input f d enabled =
    let a = [Vdom.oninput f; Vdom.value (string_of_date d)] in
    let a = if enabled then a else Vdom.attr "disabled" "" :: a in
    let a = Vdom.type_ "date" :: a in
    Vdom.input ~a []
  in
  let button =
    let a = [Vdom.type_button; Vdom.onclick (fun _ -> Clicked)] in
    let a =
      match direction with
      | One_way -> a
      | Return ->
          if start_date <= end_date then
            a
          else
            Vdom.attr "disabled" "" :: a
    in
    Vdom.elt "button" ~a [Vdom.text "Book"]
  in
  Vdom.div [
    select;
    date_input (fun s -> Start_date s) start_date true;
    date_input (fun s -> End_date s) end_date (direction = Return);
    button;
  ]

let update model = function
  | Selected direction ->
      {model with direction}
  | Start_date s ->
      {model with start_date = parse_date s}
  | End_date s ->
      {model with end_date = parse_date s}
  | Clicked ->
      assert false

let default_date =
  (2023, 05, 15)

let _ =
  let app = Vdom.simple_app ~init:(init default_date) ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
