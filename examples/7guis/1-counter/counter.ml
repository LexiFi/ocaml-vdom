type model =
  int

type msg =
  | Clicked

let init =
  0

let view n =
  Vdom.div [
    Vdom.input ~a:[Vdom.value (string_of_int n); Vdom.bool_prop "readOnly" true] [];
    Vdom.elt "button" ~a:[Vdom.type_button; Vdom.onclick (fun _ -> Clicked)]
      [Vdom.text "Count"]
  ]

let update (model : model) Clicked =
  model + 1

let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container =
    Option.get (Js_browser.Document.get_element_by_id Js_browser.document "container")
  in
  Vdom_blit.dom (Vdom_blit.run ~container app)
