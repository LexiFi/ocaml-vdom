module Window = struct
  open Js_browser

  type kind = Resize

  let key = function Resize -> "resize"

  let kind = function Resize -> Event.Resize

  type Vdom.Custom.event += WindowEvent : Event.t -> Vdom.Custom.event

  type Vdom.Custom.t += WindowListener : kind -> Vdom.Custom.t

  type listener = {
    mutable last_id: int;
    cancel: unit -> unit;
    handlers: (int, Event.t -> unit) Hashtbl.t;
  }

  let onresize f : _ Vdom.vdom =
    let handler = function WindowEvent ev -> f ev | _ -> None in
    Vdom.custom ~a:[ Vdom.oncustomevent handler ] (WindowListener Resize)

  let listeners = Hashtbl.create 2

  let new_handler event handler =
    let key = key event in
    let listener =
      match Hashtbl.find_opt listeners key with
      | None ->
          let handlers = Hashtbl.create 2 in
          let callback ev = Hashtbl.iter (fun _ f -> f ev) handlers in
          let cancel =
            Window.add_cancellable_event_listener window (kind event) callback
              true
          in
          let listener = { cancel; handlers; last_id = 0 } in
          Hashtbl.add listeners key listener;
          listener
      | Some listener -> listener
    in
    let id = listener.last_id in
    listener.last_id <- id + 1;
    Hashtbl.add listener.handlers id handler;
    fun () ->
      Hashtbl.remove listener.handlers id;
      if Hashtbl.length listener.handlers = 0 then begin
        Hashtbl.remove listeners key;
        listener.cancel ()
      end

  let handler ~send event =
    let dispose =
      new_handler event (fun x -> send (Vdom.custom_event (WindowEvent x)))
    in
    let sync ct =
      match ct with WindowListener kind -> kind = event | _ -> false
    in
    let elt = Document.create_text_node document "" in
    Vdom_blit.Custom.make ~dispose ~sync elt

  let () =
    let f ctx custom =
      let send = Vdom_blit.Custom.send_event ctx in
      match custom with
      | WindowListener kind -> Some (handler ~send kind)
      | _ -> None
    in
    Vdom_blit.(register (custom f))
end

module Tippy = struct
  type trigger = MouseEnter | Focus | Focusin | Click | Manual

  let string_of_trigger = function
    | MouseEnter -> "mouseenter"
    | Focus -> "focus"
    | Focusin -> "focusin"
    | Click -> "click"
    | Manual -> "manual"

  let string_of_triggers l = String.concat " " (List.map string_of_trigger l)

  type value = { content: string; trigger: trigger list option }

  type Vdom.Custom.t += Tippy of value

  let tooltip ?trigger content : _ Vdom.vdom =
    Vdom.custom (Tippy { content; trigger })

  let handler ~parent value =
    let inst =
      Bindings.Tippy.create parent
        {
          content = value.content;
          trigger = Option.map string_of_triggers value.trigger;
        }
    in
    let value = ref value in
    let dispose () = Bindings.Tippy.destroy inst in
    let sync ct =
      match ct with
      | Tippy new_value ->
          if new_value <> !value then begin
            value := new_value;
            Bindings.Tippy.set_content inst new_value.content;
            Bindings.Tippy.set_props inst
              { trigger = Option.map string_of_triggers new_value.trigger }
          end;
          true
      | _ -> false
    in
    let elt = Js_browser.(Document.create_text_node document "") in
    Vdom_blit.Custom.make ~dispose ~sync elt

  let () =
    let f ctx attr =
      let parent = Vdom_blit.Custom.parent ctx in
      match attr with Tippy value -> Some (handler ~parent value) | _ -> None
    in
    Vdom_blit.(register (custom f))
end
