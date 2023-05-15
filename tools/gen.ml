let read_all fn =
  let ic = open_in_bin fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let anon fn =
  Printf.printf "%S, %S;\n" (Filename.basename (Filename.dirname fn)) (read_all fn)

let () =
  print_endline "let v = [|";
  Arg.parse [] anon "";
  print_endline "|]"
