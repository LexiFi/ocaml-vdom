(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

(** {1 Bindings for the DOM and other client-side Javascript APIs} *)


module Storage : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val length: t -> int
  val key: t -> int -> string option
  val get_item: t -> string -> string option
  val set_item: t -> string -> string -> unit
  val remove_item: t -> string -> unit
  val clear: t -> unit
end

module File : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val name: t -> string
  val type_: t -> string
end

module DataTransfer : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val files : t -> File.t list
end

module Event : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val target: t -> Ojs.t
  val prevent_default: t -> unit
  val type_: t -> string

  val init_event: t -> string -> bool -> bool -> unit

  val client_x: t -> int (* mouse *)
  val client_y: t -> int (* mouse *)

  val screen_x: t -> int (* mouse *)
  val screen_y: t -> int (* mouse *)

  val buttons: t -> int  (* mouse *)

  val alt_key: t -> bool (* key *)
  val shift_key: t -> bool (* key *)
  val which: t -> int    (* key *)

  val delta_y: t -> float (* wheel *)
  val delta_x: t -> float (* wheel *)

  val data_transfer: t -> DataTransfer.t (* drag/drop*)
  val data: t -> Ojs.t (* message *)
  val origin: t -> string (* message *)
end

module Rect : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val height: t -> float
  val width: t -> float
  val left: t -> float
  val right: t -> float
  val top: t -> float
  val bottom: t -> float
end

module SVGRect : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val x: t -> float
  val y: t -> float
  val height: t -> float
  val width: t -> float
end

module Style : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val set: t -> string -> string -> unit
  [@@js.custom
     let set style prop value =
       Ojs.set (t_to_js style) prop (Ojs.string_to_js value)
  ]
  val set_color: t -> string -> unit
  val set_border: t -> string -> unit
  val set_background: t -> string -> unit
  val set_background_color: t -> string -> unit
  val set_height: t -> string -> unit
  val set_width: t -> string -> unit
  val set_bottom: t -> string -> unit
  val set_left: t -> string -> unit
  val set_top: t -> string -> unit
  val set_right: t -> string -> unit
  val set_cursor: t -> string -> unit
end

module Element : sig
  (* Only arguments marked with a "T" may be a textNode. *)
  (* When element arguments are required to be a specific element,
      it is marked with <tag> (where 'tag' is the element's tag name). *)

  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val null: t
  [@@js.custom
    let null = t_of_js Ojs.null
  ]

  val clone_node: t (* T *) -> bool -> t
  val contains: t (* T *) -> t (* T *) -> bool
  val append_child: t -> t (* T *) -> unit
  val insert_before: t -> t (* T *) -> t (* T *) -> unit
  val replace_child: t -> t (* T *) -> t (* T *) -> unit
  val remove_child: t -> t (* T *) -> unit
  val first_child: t -> t (* May return Element.null *)
  val last_child: t -> t (* May return Element.null *)
  val next_sibling: t (* T *) -> t (* May return Element.null *)

  val has_child_nodes: t (* T *) -> bool [@@js.call]
  val add_event_listener: t (* T *) -> string -> (Event.t -> unit) -> bool -> unit

  val has_attribute: t -> string -> bool
  val get_attribute: t -> string -> string
  val remove_attribute: t -> string -> unit
  val set_attribute: t -> string -> string -> unit
  val get_bounding_client_rect: t -> Rect.t [@@js.call]
  val get_bounding_box: t (* svg *) -> SVGRect.t [@@js.call "getBBox"]

  val normalize: t (* T *) -> unit

  val value: t (* <input> *) -> string
  val set_value: t (* <input> *) -> string -> unit
  val files: t (* <input> *) -> File.t list

  val selected_index: t (* <select> *) -> int

  val node_value: t (* T *) -> string
  val set_node_value: t (* T *) -> string -> unit
  val parent_node: t (* T *) -> t
  val node_name: t (* T *) -> string

  val dispatch_event: t (* T *) -> Event.t -> bool
  val style: t (* T *) -> Style.t
  val set_inner_HTML: t -> string -> unit
  val set_text_content: t -> string -> unit
  val set_class_name: t -> string -> unit

  val client_width: t -> int
  val client_height: t -> int
  val scroll_width: t -> int
  val scroll_height: t -> int
  val width: t -> int
  val height: t -> int

  val scroll_top: t -> float
  val set_scroll_top: t -> float -> unit

  val focus: t -> unit
end

module Document: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val create_element: t -> string -> Element.t
  val create_element_ns: t -> string -> string -> Element.t [@@js.call "createElementNS"]
  val create_text_node: t -> string -> Element.t
  val create_event: t -> string -> Event.t

  val get_element_by_id: t -> string -> Element.t option
  val get_elements_by_class_name: t -> string -> Element.t array

  val body: t -> Element.t

  val cookie: t -> string
  val set_cookie: t -> string -> unit
  val set_title: t -> string -> unit

  val open_: t -> ?mime_type:string -> ?history_mode:string -> unit -> unit [@@js.call "open"]
  val write: t -> string -> unit [@@js.call]
  val writeln: t -> string -> unit [@@js.call]
  val close: t -> unit
end

module History : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val length: t -> int
  val back: t -> unit
  val forward: t -> unit
  val go: t -> ([`Offset of int | `Url of string] [@js.union]) -> unit

  val replace_state: t -> Ojs.t -> string -> string -> unit
  val push_state: t -> Ojs.t -> string -> string -> unit
end

module Location: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val get_hash: unit -> string [@@js.get "location.hash"]
  val set_hash: string -> unit [@@js.set "location.hash"]

  val host: t -> string
  val set_host: t -> string -> unit

  val hostname: t -> string
  val set_hostname: t -> string -> unit

  val href: unit -> string [@@js.get "location.href"]
  val set_href: string -> unit [@@js.set "location.href"]

  val pathname: t -> string
  val set_pathname: t -> string -> unit

  val port: t -> string
  val set_port: t -> string -> unit

  val protocol: t -> string
  val set_protocol: t -> string -> unit

  val search: t -> string
  val set_search: t -> string -> unit

  val assign: t -> string -> unit [@@js.call]
  val reload: t -> ?force:bool -> unit -> unit
  val replace: t -> string -> unit [@@js.call]
end

module Window: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  type timeout_id
  val timeout_id_of_js: Ojs.t -> timeout_id
  val timeout_id_to_js: timeout_id -> Ojs.t

  val add_event_listener: t -> string -> (Event.t -> unit) -> bool -> unit
  val document: t -> Document.t
  val set_onload: t -> (unit -> unit) -> unit
  val set_interval: t -> (unit -> unit) -> int -> timeout_id
  val set_timeout: t -> (unit -> unit) -> int -> timeout_id
  val clear_timeout: t -> timeout_id -> unit
  val request_animation_frame: t -> (float -> unit) -> unit

  val open_: t -> ?url:string -> ?name:string -> ?features:string -> ?replace:bool -> unit -> unit
  val alert: t -> string -> unit

  val session_storage: t -> Storage.t option
  val local_storage: t -> Storage.t option

  val page_x_offset: t -> int
  val page_y_offset: t -> int
  val scroll_by: t -> int -> int -> unit
  val scroll_to: t -> int -> int -> unit
  val history: t -> History.t
  val location : t -> Location.t
end

module IFrame: sig
  val content_window: Element.t -> Window.t option
  val content_document: Element.t -> Document.t option
end

module JSON : sig
  val parse: string -> Ojs.t
    [@@js.global "JSON.parse"]
  val stringify: Ojs.t -> string
    [@@js.global "JSON.stringify"]
end

module FileReader : sig
  type state =
    | Empty [@js 0]
    | Loading [@js 1]
    | Done [@js 2] [@@js.enum]

  type t = private Ojs.t

  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val new_file_reader: unit -> t [@@js.new]
  val ready_state: t -> state
  val result: t -> string
  val set_onload: t -> (unit -> unit) -> unit
  val read_as_text: t -> File.t -> unit
end

module XHR: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t


  val create: unit -> t [@@js.new "XMLHttpRequest"]
  val open_: t -> string -> string -> unit
  val send: t -> string -> unit
  val set_request_header: t -> string -> string -> unit
  val get_response_header: t -> string -> string option
  val override_mime_type: t -> string -> unit
  val set_with_credentials: t -> bool -> unit (* starting from IE10 *)

  type ready_state =
    | Unsent [@js 0]
    | Opened [@js 1]
    | Headers_received [@js 2]
    | Loading [@js 3]
    | Done [@js 4]
    | Other of int [@js.default]
    [@@js.enum]

  val status: t -> int
  val ready_state: t -> ready_state
  val response_text: t -> string

  val set_onreadystatechange: t -> (unit -> unit) -> unit
end

val window: Window.t
val document: Document.t

module Canvas : sig
  type context
  val context_of_js: Ojs.t -> context
  val context_to_js: context -> Ojs.t

  type gradient
  val gradient_of_js: Ojs.t -> gradient
  val gradient_to_js: gradient -> Ojs.t

  type css_color = string

  type context_attribute = { alpha: bool; }

  val get_context:?alpha:bool -> (*<canvas>*) Element.t -> context option
  [@@js.custom
      val get_context_internal: Element.t -> string -> context_attribute -> context option
         [@@js.call "getContext"]
      let get_context ?(alpha = true) canvas =
         get_context_internal canvas "2d" {alpha}
  ]

  val to_data_URL: (*<canvas>*) Element.t -> string [@@js.call]
  val set_fill_style: context -> ([`Color of css_color | `Gradient of gradient][@js.union]) -> unit
  val set_stroke_style: context -> ([`Color of css_color | `Gradient of gradient][@js.union]) -> unit
  val set_line_width: context -> float -> unit
  val create_linear_gradient: context -> float -> float -> float -> float -> gradient
  val add_color_stop: gradient -> float -> css_color -> unit
  val begin_path: context -> unit
  val close_path: context -> unit
  val arc: context -> float -> float -> float -> float -> float -> unit
  val move_to: context -> float -> float -> unit
  val line_to: context -> float -> float -> unit
  val fill: context -> unit
  val stroke: context -> unit
  val stroke_rect: context -> float -> float -> float -> float -> unit
  val fill_rect: context -> float -> float -> float -> float -> unit
  val set_font: context -> string -> unit
  val fill_text: context -> string -> float -> float -> unit
  val stroke_text: context -> string -> float -> float -> unit
  module TextMetrics : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val width: t -> float
  end
  val measure_text: context -> string -> TextMetrics.t
  val rotate: context -> float -> unit
  val translate: context -> float -> float -> unit
  val scale: context -> float -> float -> unit
  val clear_rect: context -> float -> float -> float -> float -> unit
end

module Performance : sig
  val now: unit -> float
    [@@js.global "performance.now"]
end

module Console : sig
  type t

  val log: t -> Ojs.t -> unit
end
val console: Console.t

module Svg : sig
  module Length : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val unit_type: t -> int
    val value: t -> float
    val value_as_string: t -> string
    val value_in_specified_units: t -> float
  end

  module AnimatedLength : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val anim_val: t -> Length.t
    val base_val: t -> Length.t
  end

  type path_seg_type =
    | Unknown [@js 0]
    | Close_path [@js 1]
    | Moveto_abs [@js 2]
    | Moveto_rel [@js 3]
    | Lineto_abs [@js 4]
    | Lineto_rel [@js 5]
    | Curveto_cubic_abs [@js 6]
    | Curveto_cubic_rel [@js 7]
    | Curveto_quadratic_abs [@js 8]
    | Curveto_quadratic_rel [@js 9]
    [@@js.enum]

  module PathSeg : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val x: t -> float
    val y: t -> float
    val x1: t -> float
    val y1: t -> float
    val x2: t -> float
    val y2: t -> float

    val path_seg_type: t -> path_seg_type
    val path_seg_type_as_letter: t -> string
  end

  module PathSegList : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val number_of_items: t -> int
    val get_item: t -> int -> PathSeg.t [@@js.call]
    val insert_item_before: t -> PathSeg.t -> int -> unit [@@js.call]
    val replace_item: t -> PathSeg.t -> int -> unit [@@js.call]
    val remove_item: t -> int -> unit [@@js.call]
    val append_item: t -> PathSeg.t -> unit [@@js.call]
  end

  module PathElement: sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val path_seg_list: t -> PathSegList.t
    val normalized_path_seg_list: t -> PathSegList.t
    val animated_path_seg_list: t -> PathSegList.t
    val animated_normalized_path_seg_list: t -> PathSegList.t

    val create_close_path: t -> unit -> PathSeg.t [@@js.call "createSVGPathSegClosePath"]
    val create_moveto_abs: t -> float -> float -> PathSeg.t [@@js.call "createSVGPathSegMovetoAbs"]
    val create_moveto_rel: t -> float -> float -> PathSeg.t [@@js.call "createSVGPathSegMovetoRel"]
    val create_lineto_abs: t -> float -> float -> PathSeg.t [@@js.call "createSVGPathSegLinetoAbs"]
    val create_lineto_rel: t -> float -> float -> PathSeg.t [@@js.call "createSVGPathSegLinetoRel"]
  end
end
