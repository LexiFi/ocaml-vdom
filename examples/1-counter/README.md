## 1-counter

In this example, we build a single input field with a button. The input field
displays an integer (starting with 0) that is increased each time the button is
pressed.

The model of the app consists only of the integer in question:

```ocaml
type model =
  int
```

There is a single message that is sent when the button is clicked:

```ocaml
type msg =
  | Clicked
```

The `view` function renders the app given the current state. The button emits a
`Clicked` message when it is clicked. We also make the text field read only by
using a suitable attribute.

```ocaml
let view (model : model) =
  Vdom.div [
    Vdom.input ~a:[Vdom.value (string_of_int model); Vdom.bool_prop "readOnly" true] [];
    Vdom.elt "button" ~a:[Vdom.type_button; Vdom.onclick (fun _ -> Clicked)]
      [Vdom.text "Count"]
  ]
```

Finally, the `update` function updates the model when the `Clicked` message is
received:

```ocaml
let update (model : model) Clicked =
  model + 1
```

The tie the knot, we instantiate the app and fill the current document's body
with it:

```ocaml
let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
```
