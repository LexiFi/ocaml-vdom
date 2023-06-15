open Js_of_ocaml_toplevel
open Js_browser

let () =
  JsooTop.initialize ()

let loc_re =
  Str.regexp {|line \([0-9]+\), characters \([0-9]+\)-\([0-9]+\):|}

let add_backlink err =
  match Str.search_forward loc_re err 0 with
  | _ ->
      let link =
        let lnum = int_of_string (Str.matched_group 1 err) in
        let cnum1 = int_of_string (Str.matched_group 2 err) in
        let cnum2 = int_of_string (Str.matched_group 3 err) in
        let onclick _ =
          Window.post_message (Window.parent window)
            (Ojs.list_to_js Ojs.int_to_js [lnum; cnum1; cnum2]) "*"
        in
        let a = Document.create_element document "a" in
        Element.set_attribute a "href" "#";
        Element.add_event_listener a Click onclick true;
        Element.append_child a (Document.create_text_node document (Str.matched_string err));
        a
      in
      [
        Document.create_text_node document (String.sub err 0 (Str.match_beginning ()));
        link;
        Document.create_text_node document (String.sub err (Str.match_end ()) (String.length err - Str.match_end ()));
      ]
  | exception Not_found ->
      [Document.create_text_node document err]

let run s =
  let errbuf = Buffer.create 42 in
  let ok = JsooTop.use (Format.formatter_of_buffer errbuf) s in
  if not ok then begin
    let err = Buffer.contents errbuf in
    let body = Document.body document in
    Element.remove_all_children body;
    let pre = Document.create_element document "pre" in
    List.iter (Element.append_child pre) (add_backlink err);
    Element.append_child body pre
  end

let () =
  Window.add_event_listener window Message (fun event ->
      run (Ojs.string_of_js (Event.data event))
    ) false
