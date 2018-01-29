(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

open Js_browser
open Vdom

(* Custom commands *)

type 'msg Vdom.Cmd.t +=
  | Load_file of {file: File.t; on_success: (string -> 'msg)}

let load_file ~file on_success = Load_file {file; on_success}

(* Custom command handlers *)

let run_load_file ~file ~on_success () =
  let open FileReader in
  let r = new_file_reader () in
  set_onload r
    (fun () ->
      match ready_state r with
       | Done -> on_success (result r)
       | _ -> ()
       );
  read_as_text r file     

let cmd_handler ctx = function
  | Load_file {file; on_success} ->
      run_load_file ~file ~on_success:(fun s ->
        Vdom_blit.Cmd.send_msg ctx (on_success s)) ();
      true
  | _ -> false

let () = Vdom_blit.(register (cmd {f = cmd_handler}))

(* App *)

module LoadFile = struct
  type model = [`Empty | `Loading | `Content of string]

  let init = return `Empty

  let update model = function
    | `Load file ->
        return `Loading ~c:[load_file file (fun r -> `Loaded r)]
    | `Loaded data ->
        return (`Content data)

  let text_of_model = function
    | `Content s -> s
    | `Loading -> "Loading..."
    | _ -> ""
    
  let view model =
    div [
      input [] ~a:[
        str_prop "type" "file";
        onchange_files (fun files -> `Load (List.hd files))
      ];
      elt "hr" [];
      div [
        elt "pre" [ text (text_of_model model); ]
      ]
    ]

  let app = {init; update; view}
end

let run () =
    Vdom_blit.run LoadFile.app 
    |> Vdom_blit.dom
    |> Element.append_child (Document.body document)

let () = Window.set_onload window run




  
