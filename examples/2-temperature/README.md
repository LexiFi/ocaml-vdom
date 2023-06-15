## 2-temperature

This example consists of a simple app that converts Fahrenheit to Celsius.  It
consists of two input boxes that can be edited by the user. When the user
modifies the contents of one of them, the other one updates automatically.

The model consists of the contents of both input boxes, as strings (note that we
need to keep track of the contents of the input fields even if they are not
valid integers, so it is important to keep the contents as strings).

```ocaml
type model =
  {
    celsius: string;
    fahrenheit: string;
  }
```

The `view` function renders the two input fields together with some annotations
indicating which one is which.

```ocaml
let view {celsius; fahrenheit} =
  Vdom.div [
    Vdom.input ~a:[Vdom.oninput (fun s -> Celsius s); Vdom.value celsius] [];
    Vdom.txt_span "Celsius";
    Vdom.txt_span "=";
    Vdom.input ~a:[Vdom.oninput (fun s -> Fahrenheit s); Vdom.value fahrenheit] [];
    Vdom.txt_span "Fahrenheit"
  ]
```

The input fields carry input handlers that emit one of two messages, depending
on which input field was edited:

```ocaml
type msg =
  | Celsius of string
  | Fahrenheit of string
```

The `update` function modifies the model according to the message received:

```ocaml
let f_of_c c = c * 9/5 + 32
let c_of_f f = (f - 32) * 5/9

let update model = function
  | Celsius s ->
      begin match int_of_string_opt s with
      | None -> {model with celsius = s}
      | Some c -> {celsius = s; fahrenheit = string_of_int (f_of_c c)}
      end
  | Fahrenheit s ->
      begin match int_of_string_opt s with
      | None -> {model with fahrenheit = s}
      | Some f -> {celsius = string_of_int (c_of_f f); fahrenheit = s}
      end
```

Note that we need to handle the case when the user input is not a valid
integer. In that case, we update the model, but we cannot convert the amount so
we do not change the value of the other input field.

To finish, we initialize the app (setting the contens of both input fields to empty)
and render as the body of the current document.

```
let init =
  {celsius = ""; fahrenheit = ""}

let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
```
