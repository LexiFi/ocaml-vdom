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

type model =
  {
    undo: model option;
    redo: model option;
    selected: circle option;
    circles: circle list;
    resizing: model option;
  }

type msg =
  | Undo
  | Redo
  | Select of circle
  | Create of point
  | CreateSvg of point
  | Resize of int
  | ResizeEnd

let default_radius =
  20

let width =
  300

let height =
  200

let to_svg_point x y _ =
  let open Js_browser in
  let svg = Svg.Element.t_of_js (Element.t_to_js (Document.element_from_point document x y)) in
  let pt = Point.matrix_transform (Point.create x y) (Matrix.inverse (Svg.Element.get_screen_CTM svg)) in
  CreateSvg {x = int_of_float (Point.x pt); y = int_of_float (Point.y pt)}

let svg ~selected circles =
  let make_circle c =
    let a =
      let fill = match selected with Some circle when c == circle -> "lightgray" | _ -> "white" in
      let onclick _ = Select c in
      [
        Vdom.int_attr "cx" c.center.x;
        Vdom.int_attr "cy" c.center.y;
        Vdom.int_attr "r" c.radius;
        Vdom.attr "stroke" "black";
        Vdom.float_attr "stroke-width" 0.4;
        Vdom.attr "fill" fill;
        Vdom.onclick onclick;
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

let view {selected; circles; undo; redo; resizing = _} =
  let button name msg =
    let a = [Vdom.type_button; Vdom.onclick (fun _ -> msg)] in
    let a =
      match undo, redo, msg with
      | _, None, Redo | None, _, Undo -> Vdom.attr "disabled" "" :: a
      | _ -> a
    in
    Vdom.elt "button" ~a [Vdom.text name]
  in
  let buttons =
    let sizer =
      let a =
        let oninput s = Resize (int_of_float (float_of_string s)) in
        let onmouseup _ = ResizeEnd in
        let r = match selected with None -> 50 | Some c -> c.radius in
        [
          Vdom.attr "type" "range";
          Vdom.oninput oninput;
          Vdom.onmouseup onmouseup;
          Vdom.int_attr "min" 0;
          Vdom.int_attr "max" 100;
          Vdom.int_attr "value" r;
        ]
      in
      let a = match selected with None -> Vdom.attr "disabled" "" :: a | _ -> a in
      Vdom.input ~a []
    in
    Vdom.div [button "Undo" Undo; button "Redo" Redo; sizer]
  in
  let svg = Vdom.div [svg ~selected circles] in
  Vdom.div [buttons; svg]

type 'msg Vdom.Cmd.t +=
  | Perform of (Js_browser.Element.t -> 'msg)

let () =
  let f ctx = function
    | Perform msg ->
        Vdom_blit.Cmd.send_msg ctx (msg (Vdom_blit.Cmd.container ctx));
        true
    | _ ->
        false
  in
  Vdom_blit.register (Vdom_blit.cmd {f})

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
        | Some sel when c == sel -> None
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
  | Resize r ->
      begin match model.selected with
      | None -> Vdom.return model
      | Some sel ->
          let selected = {sel with radius = r} in
          let circles = List.map (fun c -> if c == sel then selected else c) model.circles in
          let resizing = match model.resizing with None -> Some model | Some _ as x -> x in
          Vdom.return {model with selected = Some selected; circles; resizing}
      end
  | ResizeEnd ->
      Vdom.return {model with resizing = None; undo = model.resizing}

let init =
  Vdom.return
    {
      undo = None;
      redo = None;
      selected = None;
      circles = [];
      resizing = None;
    }

let _ =
  let app = Vdom.app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
