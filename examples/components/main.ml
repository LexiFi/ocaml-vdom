[@@@ocaml.warning "-a"]

module V = Vdom

type theme = Light | Dark
let string_of_theme = function
  | Light -> "light"
  | Dark -> "dark"

let theme : theme Vdom.context = Vdom.create_context Light

module Dropdown = struct

  type model = Open | Close
  type 'msg msg = Click | Inner of 'msg

  let {Vdom.build} = Vdom.component_factory ()

  let item ?(a = []) ~active ~click txt =
    let item_classes =
      if active then
        "dropdown-item active"
      else
        "dropdown-item"
    in
    let a = Vdom.onclick (fun _ -> click) :: Vdom.class_ item_classes :: a in
    V.elt "button" ~a [V.text (String.capitalize_ascii txt)]

  let switch = function Open -> Close | Close -> Open
  let update model = function
    | Click -> Vdom.ret (switch model)
    | Inner msg -> Vdom.ret ~pub:[Vdom.Cmd.echo msg] Close

  let dropdown ?(a = []) ~title items =
    build ~key:"dropdown" ~init:Close ~update
      (fun status ->
         V.get_context theme
           (fun theme ->
              let button =
                let a = [Vdom.onclick (fun _ -> Click); V.class_ "btn btn-secondary dropdown-toggle"; V.attr "type" "button" ] in
                Vdom.elt ~key:"toggle" "button" ~a [Vdom.text title];
              in
              let menu =
                let a =
                  [Vdom.class_
                     ( match status with
                       | Open -> "dropdown-menu show"
                       | Close -> "dropdown-menu"
                     ) ]
                in
                Vdom.elt ~key:"menu" "div" ~a items |> Vdom.map (fun x -> Inner x)
              in
              let a = [Vdom.class_ ("dropdown "^(string_of_theme theme))] in
              Vdom.div ~key:"dropdown" ~a [
                button;
                menu
              ]
           )
      )

end

module Id = struct
  type model = string Lazy.t

  let {Vdom.build} : model Vdom.component_factory = Vdom.component_factory ()

  let cpt = ref 0
  let init () =
    incr cpt;
    "vdom-id-"^string_of_int !cpt

  let update model pub = V.ret ~pub:[V.Cmd.echo pub] model

  let use_id (type msg) (view: string -> msg V.vdom) : msg V.vdom =
    let init = Lazy.from_fun init in
    build ~init ~update (fun id ->
        view (Lazy.force id)
      )

end

module Tabs

: sig
    type 'msg tab
    val tabs: ?active:string -> ?active_default:string -> 'msg tab list -> 'msg Vdom.vdom
    val tab: key:string -> ?enabled:bool -> title:string -> 'msg Vdom.vdom list -> 'msg tab
  end = struct
  let {V.build} = V.component_factory ()

  let key_context = V.create_context None

  let item ~key ~active ~enabled title =
    let a = [ V.class_ "nav-link" ] in
    let a = if active then V.add_class "active" a else a in
    let a = if enabled then a else V.add_class "disabled" a in
    V.elt ~key "li" ~a:[V.class_ "nav-item"] [
      V.elt "button" ~a:(V.onclick (fun _ -> `Change (Some key)) :: a) [ V.text title ]
    ]

  type 'msg tab = {
    key: string;
    enabled: bool;
    title: string;
    content: 'msg Vdom.vdom list;
  }

  let tab ~key ?(enabled = true) ~title content =
    {
      key;
      enabled;
      title;
      content
    }

  let update model = function
    | `Change key -> V.ret key
    | `Inner msg -> V.ret ~pub:[V.Cmd.echo msg] model

  let tabs ?active ?active_default tabs =
    build ~init:active_default ~update
      (fun current ->
        let active = if active = None then current else active in
        Vdom.div ~key:"tabs" ~a:[Vdom.class_ "tabs"] [
        Vdom.elt "ul" ~a:[Vdom.class_ "nav nav-tabs"] (
          List.mapi
            (fun index {key; title; enabled; content = _} ->
               let active =
                 match active with
                 | None -> index = 0
                 | Some key' -> key = key'
               in
               item ~key ~active ~enabled title
            ) tabs
        );
        Vdom.div ~key:"tab-content" ~a:[Vdom.class_ "tab-content"] (
          List.mapi
            (fun index {key; content; _ } ->
              let a = [Vdom.class_ "content fade"] in
              let active =
                 match active with
                 | None -> index = 0
                 | Some key' -> key = key'
               in
               let a = if active then Vdom.add_class "show" a else Vdom.add_class "d-none" a in
              Vdom.div ~key ~a content
            ) tabs
        ) |> V.map (fun x -> `Inner x)
        ]
      )



end

let use_id f = Id.use_id f

type model = {
  theme: theme;
}
type msg = Change of theme

let view ({theme = current_theme} : model) =
  V.set_context theme current_theme (
    let item theme =
      let active = theme = current_theme in
      Dropdown.item ~active ~click:(Change theme)
        (string_of_theme theme)
    in
    V.div ~a:[V.attr "data-bs-theme" (string_of_theme current_theme)] [
    Tabs.tabs [
      Tabs.tab ~key:"settings" ~title:"Settings" [
            Dropdown.dropdown ~title:(String.capitalize_ascii (string_of_theme current_theme)) [
      item Light;
      item Dark
    ];
      ];
      Tabs.tab ~key:"info" ~title:"Info" [
        Vdom.elt "p" [
          Vdom.text "The quick brown fox jumps over the lazy dog."
        ]
      ]
    ]

    ]
    )

let init = V.return { theme = Light }

let update model (Change theme) =
  V.return { model with theme }
let app = V.app ~init ~view ~update ()

open Js_browser

let run () =
  let container = Document.body document in
  ignore (Vdom_blit.run ~container app)

let () = Window.set_onload window run
