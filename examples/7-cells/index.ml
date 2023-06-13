module M = Map.Make (struct type t = int * int let compare = Stdlib.compare end)

type cell =
  {
    content: string;
  }

type model =
  {
    cells: cell M.t;
  }

type msg =
  | Input of int * int * string

let num_rows =
  100

let num_cols =
  Char.code 'Z' - Char.code 'A' + 1

let init =
  let cells =
    let r = ref M.empty in
    for i = 0 to num_rows - 1 do
      for j = 0 to num_cols - 1 do
        r := M.add (i, j) {content = ""} !r
      done
    done;
    !r
  in
  Vdom.return {cells}

let update model = function
  | Input (i, j, s) ->
      Vdom.return {cells = M.update (i, j) (function Some _ -> Some {content = s} | None -> None) model.cells}

let view model =
  let make_row i =
    let make_cell j =
      let cell = M.find (i, j) model.cells in
      let input =
        let a =
          [
            Vdom.onchange (fun s -> Input (i, j, s));
          ]
        in
        Vdom.input ~a [Vdom.text cell.content]
      in
      Vdom.elt "td" [input]
    in
    let rowhead = Vdom.elt "th" ~a:[Vdom.attr "scope" "row"] [Vdom.text (string_of_int i)] in
    Vdom.elt "tr" (rowhead :: List.init num_cols make_cell)
  in
  let colhead =
    let mk i = Vdom.elt "th" ~a:[Vdom.attr "scope" "col"] [Vdom.text (String.make 1 (Char.chr (Char.code 'A' + i)))] in
    Vdom.elt "tr" (Vdom.elt "th" [] :: List.init num_cols mk)
  in
  Vdom.elt "table" (colhead :: List.init num_rows make_row)

let _ =
  let app = Vdom.app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
