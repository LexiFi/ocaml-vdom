type point =
  {
    x: int;
    y: int;
  }

type circle =
  {
    center: point;
    radius: int;
  }

type selected =
  {
    circle: circle;
    context: point option;
    adjusting: int option;
  }

type model =
  {
    undo: model option;
    redo: model option;
    selected: selected option;
    circles: circle list;
  }

type msg =
  | Undo
  | Redo
  | Select of circle
  | Create of point
  | CreateSvg of point
  | Context of circle * point
  | Adjust of circle
  | Adjusting of int

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
        | Some sel when c == sel.circle -> None
        | _ -> Some {circle = c; context = None; adjusting = None}
      in
      Vdom.return {model with selected}
  | Create {x; y} ->
      let c = [Perform (to_svg_point (float x) (float y))] in
      Vdom.return ~c model
  | CreateSvg center ->
      let circle = {center; radius = default_radius} in
      let circles = circle :: model.circles in
      Vdom.return {model with undo = Some model; redo = None; circles}
  | Context (circle, pt) ->
      Vdom.return {model with selected = Some {circle; context = Some pt; adjusting = None}}
  | Adjust circle ->
      Vdom.return {model with selected = Some {circle; context = None; adjusting = Some circle.radius}}
  | Adjusting r ->
      let selected =
        match model.selected with
        | None -> None
        | Some sel -> Some {sel with adjusting = Some r}
      in
      Vdom.return {model with selected}

let svg ~selected circles =
  let make_circle c =
    let a =
      let r = match selected with Some {circle; adjusting = Some r; _} when c == circle -> r | _ -> c.radius in
      let fill = match selected with Some {circle; _} when c == circle -> "lightgray" | _ -> "white" in
      let onclick _ = Select c in
      let oncontextmenu evt = Context (c, {x = evt.Vdom.x; y = evt.Vdom.y}) in
      [
        Vdom.int_attr "cx" c.center.x;
        Vdom.int_attr "cy" c.center.y;
        Vdom.int_attr "r" r;
        Vdom.attr "stroke" "black";
        Vdom.float_attr "stroke-width" 0.4;
        Vdom.attr "fill" fill;
        Vdom.onclick onclick;
        Vdom.oncontextmenu oncontextmenu;
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
  let rect =
    Vdom.svg_elt "rect" ~a:[
      Vdom.int_attr "width" width;
      Vdom.int_attr "height" height;
      Vdom.attr "fill" "none";
      Vdom.attr "stroke" "black";
      Vdom.float_attr "stroke-width" 0.4
    ] []
  in
  Vdom.svg_elt "svg" ~a (List.fold_left (fun accu c -> make_circle c :: accu) [rect] circles)

let view {selected; circles; undo; redo} =
  let button name msg =
    let a = [Vdom.type_button; Vdom.onclick (fun _ -> msg)] in
    let a =
      match undo, redo, msg with
      | _, None, Redo | None, _, Undo -> Vdom.attr "disabled" "" :: a
      | _ -> a
    in
    Vdom.elt "button" ~a [Vdom.text name]
  in
  let buttons = Vdom.div [button "Undo" Undo; button "Redo" Redo] in
  let svg = Vdom.div [svg ~selected circles] in
  let contextmenu =
    match selected with
    | Some {circle; context = Some _pt; _} ->
        let onclick _ = Adjust circle in
        Vdom.div ~a:[Vdom.onclick onclick] [Vdom.text "Adjust diameter..."]
    | _ ->
        Vdom.div []
  in
  let adjuster =
    match selected with
    | Some {adjusting = Some r; _} ->
        let input =
          let a =
            let oninput s = Adjusting (int_of_float (float_of_string s)) in
            [
              Vdom.attr "type" "range";
              Vdom.oninput oninput;
              Vdom.int_attr "min" 0;
              Vdom.int_attr "max" 100;
              Vdom.int_attr "value" r;
            ]
          in
          Vdom.input ~a []
        in
        Vdom.div [input]
    | _ -> Vdom.div []
  in
  Vdom.div [buttons; svg; contextmenu; adjuster]

let _ =
  let app = Vdom.app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
