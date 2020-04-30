(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

(** {1 Bindings for the DOM and other client-side Javascript APIs} *)

module Promise: sig
  [@@@js.stop]
  type 'a t
  val then_: ?error:(Ojs.t -> unit) -> success:('a -> unit) -> 'a t -> unit
  [@@@js.start]

  [@@@js.implem
    type 'a t = (Ojs.t -> 'a) * Ojs.t

    let t_of_js f x = (f, x)
    val then_: Ojs.t -> success:(Ojs.t -> unit) -> error:(Ojs.t -> unit) option -> unit[@@js.call "then"]
    let then_ ?error ~success (alpha_of_js, ojs) =
      then_ ojs ~success:(fun x -> success (alpha_of_js x)) ~error
  ]
end


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

module RegExp : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val new_reg_exp: string -> ?flags:string -> unit -> t [@@js.new]
end

module JsString : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val of_string: string -> t
  [@@js.custom let of_string s = Ojs.string_to_js s]

  val to_string: t -> string
  [@@js.custom let to_string x = Ojs.string_of_js x]

  val to_lower_case: t -> t [@@js.call]
  val to_upper_case: t -> t [@@js.call]
  val concat: t -> (t list [@js.variadic]) -> t
  val includes: t -> t -> bool
  val ends_with: t -> t -> bool
  val index_of: t -> t -> int
  val repeat: t -> int -> t
  val search: t -> RegExp.t -> int
  val trim: t -> t [@@js.call]
end

module Date : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val new_date: float -> t [@@js.new]
  val now : unit -> float [@@js.global "Date.now"]
  val parse : string -> t [@@js.global "Date.parse"]

  val get_date : t -> int [@@js.call]
  val get_day : t -> int [@@js.call]
  val get_full_year : t -> int [@@js.call]
  val get_hours : t -> int [@@js.call]
  val get_milliseconds : t -> int [@@js.call]
  val get_minutes : t -> int [@@js.call]
  val get_month : t -> int [@@js.call]
  val get_seconds : t -> int [@@js.call]
  val get_time : t -> int [@@js.call]
  val get_timezone_offset : t -> int [@@js.call]
  val get_UTC_date : t -> int [@@js.call]
  val get_UTC_day : t -> int [@@js.call]
  val get_UTC_full_year : t -> int [@@js.call]
  val get_UTC_hours : t -> int [@@js.call]
  val get_UTC_milliseconds : t -> int [@@js.call]
  val get_UTC_minutes : t -> int [@@js.call]
  val get_UTC_month : t -> int [@@js.call]
  val get_UTC_seconds : t -> int [@@js.call]
  val get_year : t -> int [@@js.call]

  val set_date : t -> int -> unit [@@js.call]
  val set_full_year : t -> int -> unit [@@js.call]
  val set_hours : t -> int -> unit [@@js.call]
  val set_milliseconds : t -> int -> unit [@@js.call]
  val set_minutes : t -> int -> unit [@@js.call]
  val set_month : t -> int -> unit [@@js.call]
  val set_seconds : t -> int -> unit [@@js.call]
  val set_time : t -> int -> unit [@@js.call]
  val set_UTC_date : t -> int -> unit [@@js.call]
  val set_UTC_full_year : t -> int -> unit [@@js.call]
  val set_UTC_hours : t -> int -> unit [@@js.call]
  val set_UTC_milliseconds : t -> int -> unit [@@js.call]
  val set_UTC_minutes : t -> int -> unit [@@js.call]
  val set_UTC_month : t -> int -> unit [@@js.call]
  val set_UTC_seconds : t -> int -> unit [@@js.call]
  val set_year : t -> int -> unit [@@js.call]

  val to_date_string : t -> string [@@js.call]
  val to_GMT_string : t -> string [@@js.call]
  val to_ISO_string : t -> string [@@js.call]
  val to_locale_string : t -> string [@@js.call]
  val to_string : t -> string [@@js.call]
  val to_time_string : t -> string [@@js.call]
  val to_UTC_string : t -> string [@@js.call]
end

module File : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val name: t -> string
  val size: t -> int
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

  (* see https://developer.mozilla.org/en-US/docs/Web/Events *)
  type kind =
    | Abort [@js "abort"]
    | Afterprint [@js "afterprint"]
    | Animationend [@js "animationend"]
    | Animationiteration [@js "animationiteration"]
    | Animationstart [@js "animationstart"]
    | Appinstalled [@js "appinstalled"]
    | Audioend [@js "audioend"]
    | Audioprocess [@js "audioprocess"]
    | Audiostart [@js "audiostart"]
    | Beforeprint [@js "beforeprint"]
    | Beforeunload [@js "beforeunload"]
    | BeginEvent [@js "beginEvent"]
    | Blocked [@js "blocked"]
    | Blur [@js "blur"]
    | Boundary [@js "boundary"]
    | Cached [@js "cached"]
    | Canplay [@js "canplay"]
    | Canplaythrough [@js "canplaythrough"]
    | Change [@js "change"]
    | Chargingchange [@js "chargingchange"]
    | Chargingtimechange [@js "chargingtimechange"]
    | Checking [@js "checking"]
    | Click [@js "click"]
    | Close [@js "close"]
    | Complete [@js "complete"]
    | Compositionend [@js "compositionend"]
    | Compositionstart [@js "compositionstart"]
    | Compositionupdate [@js "compositionupdate"]
    | Contextmenu [@js "contextmenu"]
    | Copy [@js "copy"]
    | Cut [@js "cut"]
    | Dblclick [@js "dblclick"]
    | Devicechange [@js "devicechange"]
    | Devicelight [@js "devicelight"]
    | Devicemotion [@js "devicemotion"]
    | Deviceorientation [@js "deviceorientation"]
    | Deviceproximity [@js "deviceproximity"]
    | Dischargingtimechange [@js "dischargingtimechange"]
    | DOMActivate [@js "DOMActivate"]
    | DOMAttributeNameChanged [@js "DOMAttributeNameChanged"]
    | DOMAttrModified [@js "DOMAttrModified"]
    | DOMCharacterDataModified [@js "DOMCharacterDataModified"]
    | DOMContentLoaded [@js "DOMContentLoaded"]
    | DOMElementNameChanged [@js "DOMElementNameChanged"]
    | DOMFocusIn [@js "DOMFocusIn"]
    | DOMFocusOut [@js "DOMFocusOut"]
    | DOMNodeInserted [@js "DOMNodeInserted"]
    | DOMNodeInsertedIntoDocument [@js "DOMNodeInsertedIntoDocument"]
    | DOMNodeRemoved [@js "DOMNodeRemoved"]
    | DOMNodeRemovedFromDocument [@js "DOMNodeRemovedFromDocument"]
    | DOMSubtreeModified [@js "DOMSubtreeModified"]
    | Downloading [@js "downloading"]
    | Drag [@js "drag"]
    | Dragend [@js "dragend"]
    | Dragenter [@js "dragenter"]
    | Dragleave [@js "dragleave"]
    | Dragover [@js "dragover"]
    | Dragstart [@js "dragstart"]
    | Drop [@js "drop"]
    | Durationchange [@js "durationchange"]
    | Emptied [@js "emptied"]
    | End [@js "end"]
    | Ended [@js "ended"]
    | EndEvent [@js "endEvent"]
    | Error [@js "error"]
    | Focus [@js "focus"]
    | Focusin [@js "focusin"]
    | Focusout [@js "focusout"]
    | Fullscreenchange [@js "fullscreenchange"]
    | Fullscreenerror [@js "fullscreenerror"]
    | Gamepadconnected [@js "gamepadconnected"]
    | Gamepaddisconnected [@js "gamepaddisconnected"]
    | Gotpointercapture [@js "gotpointercapture"]
    | Hashchange [@js "hashchange"]
    | Input [@js "input"]
    | Invalid [@js "invalid"]
    | Keydown [@js "keydown"]
    | Keypress [@js "keypress"]
    | Keyup [@js "keyup"]
    | Languagechange [@js "languagechange"]
    | Levelchange [@js "levelchange"]
    | Load [@js "load"]
    | Loadeddata [@js "loadeddata"]
    | Loadedmetadata [@js "loadedmetadata"]
    | Loadend [@js "loadend"]
    | Loadstart [@js "loadstart"]
    | Lostpointercapture [@js "lostpointercapture"]
    | Mark [@js "mark"]
    | Message [@js "message"]
    | Messageerror [@js "messageerror"]
    | Mousedown [@js "mousedown"]
    | Mouseenter [@js "mouseenter"]
    | Mouseleave [@js "mouseleave"]
    | Mousemove [@js "mousemove"]
    | Mouseout [@js "mouseout"]
    | Mouseover [@js "mouseover"]
    | Mouseup [@js "mouseup"]
    | Nomatch [@js "nomatch"]
    | Notificationclick [@js "notificationclick"]
    | Noupdate [@js "noupdate"]
    | Obsolete [@js "obsolete"]
    | Offline [@js "offline"]
    | Online [@js "online"]
    | Open [@js "open"]
    | Orientationchange [@js "orientationchange"]
    | Pagehide [@js "pagehide"]
    | Pageshow [@js "pageshow"]
    | Paste [@js "paste"]
    | Pause [@js "pause"]
    | Play [@js "play"]
    | Playing [@js "playing"]
    | Pointercancel [@js "pointercancel"]
    | Pointerdown [@js "pointerdown"]
    | Pointerenter [@js "pointerenter"]
    | Pointerleave [@js "pointerleave"]
    | Pointerlockchange [@js "pointerlockchange"]
    | Pointerlockerror [@js "pointerlockerror"]
    | Pointermove [@js "pointermove"]
    | Pointerout [@js "pointerout"]
    | Pointerover [@js "pointerover"]
    | Pointerup [@js "pointerup"]
    | Popstate [@js "popstate"]
    | Progress [@js "progress"]
    | Push [@js "push"]
    | Pushsubscriptionchange [@js "pushsubscriptionchange"]
    | Ratechange [@js "ratechange"]
    | Readystatechange [@js "readystatechange"]
    | RepeatEvent [@js "repeatEvent"]
    | Reset [@js "reset"]
    | Resize [@js "resize"]
    | Resourcetimingbufferfull [@js "resourcetimingbufferfull"]
    | Result [@js "result"]
    | Resume [@js "resume"]
    | Scroll [@js "scroll"]
    | Seeked [@js "seeked"]
    | Seeking [@js "seeking"]
    | Select [@js "select"]
    | Selectionchange [@js "selectionchange"]
    | Selectstart [@js "selectstart"]
    | Show [@js "show"]
    | Slotchange [@js "slotchange"]
    | Soundend [@js "soundend"]
    | Soundstart [@js "soundstart"]
    | Speechend [@js "speechend"]
    | Speechstart [@js "speechstart"]
    | Stalled [@js "stalled"]
    | Start [@js "start"]
    | Storage [@js "storage"]
    | Submit [@js "submit"]
    | Success [@js "success"]
    | Suspend [@js "suspend"]
    | SVGAbort [@js "SVGAbort"]
    | SVGError [@js "SVGError"]
    | SVGLoad [@js "SVGLoad"]
    | SVGResize [@js "SVGResize"]
    | SVGScroll [@js "SVGScroll"]
    | SVGUnload [@js "SVGUnload"]
    | SVGZoom [@js "SVGZoom"]
    | Timeout [@js "timeout"]
    | Timeupdate [@js "timeupdate"]
    | Touchcancel [@js "touchcancel"]
    | Touchend [@js "touchend"]
    | Touchmove [@js "touchmove"]
    | Touchstart [@js "touchstart"]
    | Transitionend [@js "transitionend"]
    | Unload [@js "unload"]
    | Updateready [@js "updateready"]
    | Upgradeneeded [@js "upgradeneeded"]
    | Userproximity [@js "userproximity"]
    | Versionchange [@js "versionchange"]
    | Visibilitychange [@js "visibilitychange"]
    | Voiceschanged [@js "voiceschanged"]
    | Volumechange [@js "volumechange"]
    | Waiting [@js "waiting"]
    | Wheel [@js "wheel"]
    | NonStandard of string [@js.default]
  [@@js.enum]

  val target: t -> Ojs.t
  val prevent_default: t -> unit
  val type_: t -> string

  val init_event: t -> kind -> bool -> bool -> unit

  val client_x: t -> int (* mouse *)
  val client_y: t -> int (* mouse *)

  val page_x: t -> float (* mouse *)
  val page_y: t -> float (* mouse *)

  val screen_x: t -> int (* mouse *)
  val screen_y: t -> int (* mouse *)

  val movement_x: t -> int (* mouse *)
  val movement_y: t -> int (* mouse *)

  val buttons: t -> int  (* mouse *)

  val alt_key: t -> bool (* key *)
  val ctrl_key: t -> bool (* key *)
  val shift_key: t -> bool (* key *)
  val which: t -> int    (* key *)
  val code: t -> string (* key *)
  val key: t -> string (* key *)

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
  val set_position: t -> string -> unit
  val set_cursor: t -> string -> unit
  val set_display: t -> string -> unit

  val get: t -> string -> string
  [@@js.custom
    let get style prop =
      Ojs.string_of_js (Ojs.get (t_to_js style) prop)
    ]
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

  val remove_all_children: t -> unit
  [@@js.custom
    let remove_all_children x =
      let rec loop child =
        if child = null then ()
        else (remove_child x child; loop (first_child x))
      in
      loop (first_child x)
    ]

  val has_child_nodes: t (* T *) -> bool [@@js.call]
  val add_event_listener: t (* T *) -> Event.kind -> (Event.t -> unit) -> bool -> unit
  val add_cancellable_event_listener: t -> Event.kind -> (Event.t -> unit) -> bool -> (unit -> unit)
  [@@js.custom
    val add_event_listener_internal: t -> Event.kind -> Ojs.t -> bool -> unit
    [@@js.call "addEventListener"]
    val remove_event_listener_internal: t -> Event.kind -> Ojs.t -> bool -> unit
    [@@js.call "removeEventListener"]
    let add_cancellable_event_listener x k f c =
      let f = Ojs.fun_to_js 1 (fun x -> f (Event.t_of_js x)) in
      add_event_listener_internal x k f c;
      fun () ->
        remove_event_listener_internal x k f c
                                                                                      ]
  val inner_text: t -> string
  val get_elements_by_tag_name: t -> string -> t array
  val get_elements_by_class_name: t -> string -> t array

  val has_attribute: t -> string -> bool
  val get_attribute: t -> string -> string
  val remove_attribute: t -> string -> unit
  val set_attribute: t -> string -> string -> unit
  val get_bounding_client_rect: t -> Rect.t [@@js.call]
  val get_bounding_box: t (* svg *) -> SVGRect.t [@@js.call "getBBox"]

  val normalize: t (* T *) -> unit

  val value: t (* <input> *) -> string
  val set_value: t (* <input> *) -> string -> unit
  val select: t (* <input> <textarea *) -> unit
  val files: t (* <input> *) -> File.t list

  val selected_index: t (* <select> *) -> int
  val checked: t (* <input> *) -> bool
  val set_checked: t (* <input> *) -> bool -> unit

  val node_value: t (* T *) -> string
  val set_node_value: t (* T *) -> string -> unit
  val parent_node: t (* T *) -> t
  val node_name: t (* T *) -> string

  val dispatch_event: t (* T *) -> Event.t -> bool
  val style: t (* T *) -> Style.t
  val set_inner_HTML: t -> string -> unit
  val set_text_content: t -> string -> unit
  val set_class_name: t -> string -> unit
  val class_name: t -> string

  val client_width: t -> int
  val client_height: t -> int
  val scroll_width: t -> int
  val scroll_height: t -> int
  val width: t -> int
  val height: t -> int

  val offset_parent: t -> t option
  val offset_top: t -> int
  val offset_left: t -> int
  val offset_width: t -> int
  val offset_height: t -> int

  val scroll_top: t -> float
  val set_scroll_top: t -> float -> unit

  val focus: t -> unit
  val blur: t -> unit

  val selection_start: t -> int
  val selection_end: t -> int
  val set_selection_start: t -> int -> unit
  val set_selection_end: t -> int -> unit

  val remove: t -> unit
  val click: t -> unit
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
  val document_element: t -> Element.t
  val active_element: t -> Element.t

  val cookie: t -> string
  val set_cookie: t -> string -> unit
  val set_title: t -> string -> unit

  val open_: t -> ?mime_type:string -> ?history_mode:string -> unit -> unit [@@js.call "open"]
  val write: t -> string -> unit [@@js.call]
  val writeln: t -> string -> unit [@@js.call]
  val close: t -> unit

  val exec_command: t -> string -> bool

  val query_selector: t -> string -> Element.t

  val remove_all_selection_ranges: t -> unit [@@js.call "getSelection().removeAllRanges"]
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
  type interval_id

  val add_event_listener: t -> Event.kind -> (Event.t -> unit) -> bool -> unit
  val add_cancellable_event_listener: t -> Event.kind -> (Event.t -> unit) -> bool -> (unit -> unit)
  [@@js.custom
    val add_event_listener_internal: t -> Event.kind -> Ojs.t -> bool -> unit
    [@@js.call "addEventListener"]
    val remove_event_listener_internal: t -> Event.kind -> Ojs.t -> bool -> unit
    [@@js.call "removeEventListener"]
    let add_cancellable_event_listener x k f c =
      let f = Ojs.fun_to_js 1 (fun x -> f (Event.t_of_js x)) in
      add_event_listener_internal x k f c;
      fun () ->
        remove_event_listener_internal x k f c
                                                                                      ]
  val document: t -> Document.t
  val set_onload: t -> (unit -> unit) -> unit
  val set_interval: t -> (unit -> unit) -> int -> interval_id
  val set_timeout: t -> (unit -> unit) -> int -> timeout_id
  val clear_timeout: t -> timeout_id -> unit
  val clear_interval: t -> interval_id -> unit
  val request_animation_frame: t -> (float -> unit) -> unit

  val open_: t -> ?url:string -> ?name:string -> ?features:string -> ?replace:bool -> unit -> t
  val alert: t -> string -> unit

  val session_storage: t -> Storage.t option
  val local_storage: t -> Storage.t option

  val inner_width: t -> float
  val inner_height: t -> float
  val page_x_offset: t -> int
  val page_y_offset: t -> int
  val scroll_by: t -> int -> int -> unit
  val scroll_to: t -> int -> int -> unit
  val history: t -> History.t
  val location : t -> Location.t

  val frame_element: t -> Element.t

  val get_computed_style: t -> Element.t -> Style.t

  val decode_URI_component: t -> string -> string

  val event_source: Event.t -> t [@@js.get "source"] (* message *)
  val post_message: t -> Ojs.t -> string -> unit
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
  val create: unit -> t [@@js.new "FileReader"]
  val ready_state: t -> state
  val result: t -> string
  val set_onload: t -> (unit -> unit) -> unit
  val read_as_binary_string: t -> File.t -> unit
  val read_as_text: t -> File.t -> unit
end

module XHR: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t


  val create: unit -> t [@@js.new "XMLHttpRequest"]
  val open_: t -> string -> string -> unit
  val send: t -> Ojs.t -> unit
  val set_request_header: t -> string -> string -> unit
  val get_response_header: t -> string -> string option
  val set_response_type: t -> string -> unit
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
  val response: t -> Ojs.t

  val set_onreadystatechange: t -> (unit -> unit) -> unit
end

module WebSocket : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  type ready_state =
    | Connecting [@js 0]
    | Open [@js 1]
    | Closing [@js 2]
    | Closed [@js 3]
  [@@js.enum]

  val create : string -> ?protocols:string list -> unit -> t [@@js.new "WebSocket"]
  val send : t -> string -> unit
  val close : t -> ?code:int -> ?reason:string -> unit -> unit

  val binary_type : t -> string
  val set_binary_type : t -> string -> unit
  val ready_state : t -> ready_state

  val add_event_listener: t -> Event.kind -> (Event.t -> unit) -> bool -> unit

  module CloseEvent : sig
    type t = Event.t

    val code : t -> int
  end
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

module ArrayBuffer : sig
  type t
  val create: int -> t [@@js.new "ArrayBuffer"]
end

module Uint8Array : sig
  type t
  val from_buffer: ArrayBuffer.t -> t [@@js.new "Uint8Array"]
  val set: t -> int array -> int -> unit
end

module Blob : sig
  type options
  val options: ?type_:string -> ?endings:string -> unit -> options [@@js.builder]

  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val create: ([`ArrayBuffer of ArrayBuffer.t | `Other of Ojs.t] [@js.union]) list -> ?options:options -> unit -> t [@@js.new "Blob"]
  val text: t -> unit -> string Promise.t
end

module ObjectURL : sig
  val of_blob: Blob.t -> string [@@js.global "URL.createObjectURL"]
  val revoke: string -> unit [@@js.global "URL.revokeObjectURL"]
end


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
