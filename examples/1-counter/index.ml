type model =
  int

type msg =
  | Clicked

let init =
  0

let view (model : model) =
  Vdom.div [
    Vdom.input ~a:[Vdom.value (string_of_int model); Vdom.bool_prop "readOnly" true] [];
    Vdom.elt "button" ~a:[Vdom.type_button; Vdom.onclick (fun _ -> Clicked)]
      [Vdom.text "Count"]
  ]

let update (model : model) Clicked =
  model + 1

let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
