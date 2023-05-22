type 'a point =
  {
    x: 'a;
    y: 'a;
  }

type circle =
  {
    center: int point;
    radius: int;
  }

type model =
  {
    undo: model option;
    redo: model option;
    selected: circle option;
    circles: circle list;
  }

type msg =
  | Undo
  | Redo
  | Select of circle
  | Create of int point
  | CreateSvg of int point

type 'msg Vdom.Cmd.t +=
  | Perform of (Js_browser.Element.t -> 'msg)

module Perform = struct
  let f ctx = function
    | Perform msg ->
        Vdom_blit.Cmd.send_msg ctx (msg (Vdom_blit.Cmd.container ctx));
        true
    | _ ->
        false

  let () =
    Vdom_blit.register (Vdom_blit.cmd {f})
end

let default_radius =
  20

let width =
  300

let height =
  200

let init =
  Vdom.return
    {
      undo = None;
      redo = None;
      selected = None;
      circles = [];
    }

let to_svg_point x y _ =
  let open Js_browser in
  let svg = Svg.Element.t_of_js (Element.t_to_js (Document.element_from_point document x y)) in
  let pt = Point.matrix_transform (Point.create x y) (Matrix.inverse (Svg.Element.get_screen_CTM svg)) in
  CreateSvg {x = int_of_float (Point.x pt); y = int_of_float (Point.y pt)}

let update model = function
  | Undo ->
      let model =
        match model.undo with
        | None -> model
        | Some model' -> {model' with redo = Some model}
      in
      Vdom.return model
  | Redo ->
      let model =
        match model.redo with
        | None -> model
        | Some model' -> {model' with undo = Some model}
      in
      Vdom.return model
  | Select c ->
      let selected =
        match model.selected with
        | Some c' when c == c' -> None
        | _ -> Some c
      in
      Vdom.return {model with selected}
  | Create {x; y} ->
      let c = [Perform (to_svg_point (float x) (float y))] in
      Vdom.return ~c model
  | CreateSvg center ->
      let circle = {center; radius = default_radius} in
      let circles = circle :: model.circles in
      Vdom.return {model with undo = Some model; redo = None; circles}

let svg ~selected circles =
  let make_circle c =
    let a =
      [
        Vdom.int_attr "cx" c.center.x;
        Vdom.int_attr "cy" c.center.y;
        Vdom.int_attr "r" c.radius;
        Vdom.attr "stroke" "black";
        Vdom.float_attr "stroke-width" 0.4;
        Vdom.attr "fill"
          (match selected with Some c' when c == c' -> "lightgray" | _ -> "white");
        Vdom.onclick (fun _ -> Select c);
      ]
    in
    Vdom.svg_elt "circle" ~a []
  in
  let a =
    [
      Vdom.attr "viewBox" (Printf.sprintf "0 0 %d %d" width height);
      Vdom.onclick (fun evt -> Create {x = evt.Vdom.x; y = evt.Vdom.y});
    ]
  in
  Vdom.svg_elt "svg" ~a (List.rev_map make_circle circles)

let view {selected; circles; undo; redo; _} =
  let button name msg =
    let a = [Vdom.type_button; Vdom.onclick (fun _ -> msg)] in
    let a =
      match undo, redo, msg with
      | _, None, Redo | None, _, Undo -> Vdom.attr "disabled" "" :: a
      | _ -> a
    in
    Vdom.elt "button" ~a [Vdom.text name]
  in
  Vdom.div
    [
      Vdom.div [button "Undo" Undo; button "Redo" Redo];
      Vdom.div [svg ~selected circles];
    ]

let _ =
  let app = Vdom.app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
