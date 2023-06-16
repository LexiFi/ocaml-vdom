type model =
  {
    prefix: string;
    name: string;
    surname: string;
    list: (string * string) list;
    selected: int option;
  }

type msg =
  | Prefix of string
  | Name of string
  | Surname of string
  | Select of int
  | Create
  | Update
  | Delete

let view {prefix; name; surname; list; selected} =
  let button label msg enabled =
    let a = [Vdom.type_button; Vdom.onclick (fun _ -> msg)] in
    let a = if enabled then a else Vdom.attr "disabled" "" :: a in
    Vdom.elt "button" ~a [Vdom.text label]
  in
  Vdom.div
    [
      Vdom.div [Vdom.txt_span "Filter prefix:"; Vdom.input ~a:[Vdom.value prefix; Vdom.oninput (fun s -> Prefix s)] []];
      Vdom.div [
        let options =
          List.map (fun (name, surname) ->
              Vdom.elt "option" [Vdom.text (Printf.sprintf "%s, %s" surname name)]
            ) list
        in
        let a = [Vdom.int_attr "size" 10; Vdom.onchange_index (fun i -> Select i)] in
        Vdom.elt "select" ~a options
      ];
      Vdom.div
        [
          Vdom.div [Vdom.txt_span "Name:"; Vdom.input ~a:[Vdom.value name; Vdom.oninput (fun s -> Name s)] []];
          Vdom.div [Vdom.txt_span "Surname:"; Vdom.input ~a:[Vdom.value surname; Vdom.oninput (fun s -> Surname s)] []];
        ];
      Vdom.div [button "Create" Create true; button "Update" Update (selected <> None); button "Delete" Delete (selected <> None)];
    ]

let update model = function
  | Prefix prefix ->
      {model with prefix}
  | Name name ->
      {model with name}
  | Surname surname ->
      {model with surname}
  | Select i ->
      let name, surname = List.nth model.list i in
      {model with name; surname; selected = Some i}
  | Create ->
      {model with list = (model.name, model.surname) :: model.list}
  | Update ->
      begin match model.selected with
      | None -> model
      | Some i ->
          {model with list = List.mapi (fun j s -> if i = j then (model.name, model.surname) else s) model.list}
      end
  | Delete ->
      begin match model.selected with
      | None -> model
      | Some i ->
          let j = ref (-1) in
          {model with list = List.filter (fun _ -> incr j; !j <> i) model.list}
      end

let init =
  {
    prefix = "";
    name = "";
    surname = "";
    list = [];
    selected = None;
  }

let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
