open Js_of_ocaml_toplevel

let editor =
  Ojs.get_prop_ascii (Ojs.get_prop_ascii Ojs.global "window") "editor"

let get_value () =
  let state = Ojs.get_prop_ascii editor "state" in
  Ojs.string_of_js (Ojs.call state "sliceDoc" [||])

let set_value s =
  let tr =
    let changes =
      let x = Ojs.empty_obj () in
      Ojs.set_prop_ascii x "from" (Ojs.int_to_js 0);
      let length =
        Ojs.int_of_js
          (Ojs.get_prop_ascii
             (Ojs.get_prop_ascii
                (Ojs.get_prop_ascii editor "state") "doc") "length")
      in
      Ojs.set_prop_ascii x "to" (Ojs.int_to_js length);
      Ojs.set_prop_ascii x "insert" (Ojs.string_to_js s);
      x
    in
    let x = Ojs.empty_obj () in
    Ojs.set_prop_ascii x "changes" changes;
    x
  in
  ignore (Ojs.call editor "dispatch" [|tr|] : Ojs.t)

let run _ =
  let txt = get_value () in
  (* let null = open_out "/dev/null" in *)
  let ppf = Format.std_formatter in (* Format.formatter_of_out_channel null in *)
  ignore (JsooTop.use ppf txt : bool)

let () =
  JsooTop.initialize ()

let () =
  match Js_browser.Document.get_element_by_id Js_browser.document "examples" with
  | None -> ()
  | Some examples ->
      let option (name, _) =
        let elt = Js_browser.Document.create_element Js_browser.document "option" in
        Js_browser.Element.append_child elt (Js_browser.Document.create_text_node Js_browser.document name);
        elt
      in
      let onchange evt =
        let target = Js_browser.Event.target evt in
        let idx = Ojs.int_of_js (Ojs.get_prop_ascii target "selectedIndex") in
        set_value (snd Examples.v.(idx))
      in
      Array.iter (fun v -> Js_browser.Element.append_child examples (option v)) Examples.v;
      Js_browser.Element.add_event_listener examples Js_browser.Event.Change onchange true

let () =
  match Js_browser.Document.get_element_by_id Js_browser.document "run" with
  | None -> ()
  | Some button ->
      Js_browser.Element.add_event_listener button Click run true
