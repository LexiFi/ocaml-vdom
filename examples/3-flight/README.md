## 3-flight

In this example we build a little app that lets the user enter the dates of a
trip which may be one-way or two-way. The date field for the return is only
enabled for two-way trips. Moreover, in this case, the `Book` button is disabled
if the return date is _before_ the start date.

The model consists of the contents of the two date input fields and the choice
between one-way and two-way. Note that as we will be using a [date
picker](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date),
we do not need to handle the case of invalid input, as it is impossible to
invalidate the contents of such an input field. Thus we are free to represent
dates as triples `(yyyy, mm, dd)`.

```ocaml
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
```

For the `view` function we take the model and render a piece of virtual DOM:

```ocaml
let string_of_date (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

let view {direction; start_date; end_date} =
  (* ... *)
  Vdom.div [
    select;
    date_input (fun s -> Start_date s) start_date true;
    date_input (fun s -> End_date s) end_date (direction = Return);
    button;
  ]
```

Here, `select` is a dropdown menu to choose between one-way or two-way:

```
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
```

Next, come the two date inputs, also created with an auxiliary function, where
`f` specifies the message emitted when the field is edited, and `enabled`
indicates whether the field should be editable or not.

```ocaml
let date_input f d enabled =
  let a = [Vdom.oninput f; Vdom.value (string_of_date d)] in
  let a = if enabled then a else Vdom.attr "disabled" "" :: a in
  let a = Vdom.type_ "date" :: a in
  Vdom.input ~a []
```

Finally, the `Book` button is built, making sure to disable it if the start date
comes after the return date in a two-way trip:

```ocaml
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
```

Next comes the `update` function, which handles the interaction with the user by
suitable updating the model in response to the messages coming from the date
inputs and the button.

```ocaml
type msg =
  | Selected of direction
  | Start_date of string
  | End_date of string
  | Clicked

let parse_date s =
  Scanf.sscanf s "%04d-%02d-%02d" (fun y m d -> (y, m, d))

let update model = function
  | Selected direction ->
      Vdom.return {model with direction}
  | Start_date s ->
      Vdom.return {model with start_date = parse_date s}
  | End_date s ->
      Vdom.return {model with end_date = parse_date s}
  | Clicked ->
      Vdom.return ~c:[Alert "Your flight has been booked"] model
```

We would like to show a dialog box when the user clicks the button. To do this,
we define a custom command `Alert s` and a handler for it:

```ocaml
type _ Vdom.Cmd.t +=
  | Alert of string

let () =
  let f ctx = function
    | Alert s -> Js_browser.Window.alert Js_browser.window s true
    | _ -> false
  in
  Vdom_blit.register (Vdom_blit.cmd {Vdom_blit.Cmd.f})
```

Finally, we render the app and hook it to the body of the current document:

```ocaml
let init =
  let date = (2023, 05, 15) in
  Vdom.return
    {
      direction = One_way;
      start_date = date;
      end_date = date;
    }

let _ =
  let app = Vdom.app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
```
