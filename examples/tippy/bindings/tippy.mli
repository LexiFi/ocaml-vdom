type t

val t_of_js : Ojs.t -> t

val t_to_js : t -> Ojs.t

type props = { trigger: string option }

type options = { content: string; trigger: string option }

val create : Js_browser.Element.t -> options -> t [@@js.global "tippy"]

val set_content : t -> string -> unit [@@js.call "setContent"]

val set_props : t -> props -> unit [@@js.call "setProps"]

val destroy : t -> unit
