open Js_of_ocaml_toplevel
open Js_browser

let () =
  JsooTop.initialize ()

let run s =
  let errbuf = Buffer.create 42 in
  let ok = JsooTop.use (Format.formatter_of_buffer errbuf) s in
  if not ok then begin
    let err = Buffer.contents errbuf in
    let body = Document.body document in
    Element.remove_all_children body;
    let pre = Document.create_element document "pre" in
    Element.append_child pre (Document.create_text_node document err);
    Element.append_child body pre
  end

let () =
  Window.add_event_listener window Message (fun event ->
      run (Ojs.string_of_js (Event.data event))
    ) false
