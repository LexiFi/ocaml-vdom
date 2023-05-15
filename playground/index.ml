open Js_of_ocaml_toplevel

let get_value () =
  let editor = Ojs.get_prop_ascii (Js_browser.Window.t_to_js Js_browser.window) "editor" in
  let state = Ojs.get_prop_ascii editor "state" in
  Ojs.string_of_js (Ojs.call state "sliceDoc" [||])

let run _ =
  let txt = get_value () in
  (* let null = open_out "/dev/null" in *)
  let ppf = Format.std_formatter in (* Format.formatter_of_out_channel null in *)
  ignore (JsooTop.use ppf txt : bool)

let () =
  JsooTop.initialize ()

let () =
  match Js_browser.Document.get_element_by_id Js_browser.document "run" with
  | None -> ()
  | Some button ->
      Js_browser.Element.add_event_listener button Click run true
