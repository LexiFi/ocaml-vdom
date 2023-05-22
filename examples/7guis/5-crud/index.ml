type model =
  {
    started: float;
    now: float;
    duration: float;
    max: float;
  }

type msg =
  | Tick of float
  | Reset
  | Restart of float
  | Range of float

module Time = struct
  type 'msg Vdom.Cmd.t +=
    | Now of (float -> 'msg)
    | Every of int (* msec *) * (float -> 'msg)

  let f ctx = function
    | Now msg ->
        Vdom_blit.Cmd.send_msg ctx (msg (Js_browser.Date.now ()));
        true
    | Every (d, msg) ->
        ignore (Js_browser.Window.set_interval Js_browser.window (fun () -> Vdom_blit.Cmd.send_msg ctx (msg (Js_browser.Date.now ()))) d);
        true
    | _ ->
        false

  let () =
    Vdom_blit.register (Vdom_blit.cmd {f})
end

let init duration max =
  let c = [Time.Now (fun now -> Restart now); Time.Every (100, (fun now -> Tick now))] in
  Vdom.return ~c
    {
      started = 0.0;
      now = 0.0;
      duration = duration *. 1000.;
      max = max *. 1000.;
    }

let update model = function
  | Reset ->
      Vdom.return ~c:[Time.Now (fun now -> Restart now)] model
  | Restart now ->
      Vdom.return {model with started = now; now}
  | Tick now ->
      Vdom.return {model with now}
  | Range duration ->
      Vdom.return {model with duration}

let view {started; now; duration; max} =
  let elapsed = now -. started in
  Vdom.div
    [
      Vdom.div [Vdom.txt_span "Elapsed time:";
                Vdom.elt "progress" ~a:[Vdom.float_attr "value" elapsed; Vdom.float_attr "max" duration] []];
      Vdom.div [Vdom.text (Printf.sprintf "%.1fs" (elapsed /. 1000.))];
      Vdom.div [Vdom.txt_span "Duration:";
                Vdom.input ~a:[Vdom.oninput (fun s -> Range (float_of_string s)); Vdom.attr "type" "range"; Vdom.float_attr "min" 0.0; Vdom.float_attr "max" max; Vdom.float_attr "value" duration] []];
      Vdom.div [Vdom.elt "button" ~a:[Vdom.type_button; Vdom.onclick (fun _ -> Reset)] [Vdom.text "Reset"]];
    ]

let _ =
  let app = Vdom.app ~init:(init 60.0 120.) ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
