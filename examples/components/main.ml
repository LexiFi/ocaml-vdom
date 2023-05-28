[@@@ocaml.warning "-a"]

module V = Vdom

type model = int
type msg = Click

let button msg label =
  V.input ~a:[
    V.type_button;
    V.value label;
    V.onclick (fun _ -> msg)
  ] []

let t = V.text

let br = V.elt "br" []

let p = V.elt "p"

let col ?(a = []) = V.div ~a:(V.add_class "column" a)

let row ?(a = []) = V.div ~a:(V.add_class "row" a)

let test_fragment n =
  V.fragment [
    V.fragment [
      V.text "A";
      V.text "B";
      V.text "C"
    ];
    V.fragment (List.init n (fun k -> Vdom.div ~a:[Vdom.class_ "num"]  [Vdom.text (string_of_int k)]))
     ; V.fragment [
      V.div [
        V.text "A";
        V.text "B";
        V.text "C"
      ]
    ] ]


let view (model : model) =
  V.div ~a:[Vdom.class_ "ctx"] [
    Vdom.div ~a:[Vdom.class_ "cpt"] [Vdom.text (string_of_int model)];
    button Click "Click";
    test_fragment model
  ]

let init = V.return 0

let update model Click = V.return (model + 1)

let app = V.app ~init ~view ~update ()

open Js_browser

let run () =
  let container = Document.body document in
  ignore (Vdom_blit.run ~container app)

let () = Window.set_onload window run
