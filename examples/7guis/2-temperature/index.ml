type model =
  {
    celsius: string;
    fahrenheit: string;
  }

type msg =
  | Celsius of string
  | Fahrenheit of string

let init =
  {
    celsius = "";
    fahrenheit = "";
  }

let view {celsius; fahrenheit} =
  Vdom.div [
    Vdom.input ~a:[Vdom.oninput (fun s -> Celsius s); Vdom.value celsius] [];
    Vdom.txt_span "Celsius";
    Vdom.txt_span "=";
    Vdom.input ~a:[Vdom.oninput (fun s -> Fahrenheit s); Vdom.value fahrenheit] [];
    Vdom.txt_span "Fahrenheit"
  ]

let f_of_c c =
  c * 9/5 + 32

let c_of_f f =
  (f - 32) * 5/9

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

let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container =
    Option.get (Js_browser.Document.get_element_by_id Js_browser.document "container")
  in
  Vdom_blit.dom (Vdom_blit.run ~container app)
