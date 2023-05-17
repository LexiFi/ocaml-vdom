open Js_of_ocaml_toplevel
open Js_browser

let () =
  JsooTop.initialize ()

let run s =
  (* Option.iter Element.remove_all_children (Document.get_element_by_id document "container"); *)
  (* let null = open_out "/dev/null" in *)
  let ppf = Format.std_formatter in (* Format.formatter_of_out_channel null in *)
  ignore (JsooTop.use ppf s : bool)

let () =
  Window.add_event_listener window Message (fun event ->
      run (Ojs.string_of_js (Event.data event))
    ) false
