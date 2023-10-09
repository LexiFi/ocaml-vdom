(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2023 LexiFi                                                                    *)

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
  val length: t -> int [@@js.get]
  val key: t -> int -> string option [@@js.call]
  val get_item: t -> string -> string option [@@js.call]
  val set_item: t -> string -> string -> unit [@@js.call]
  val remove_item: t -> string -> unit [@@js.call]
  val clear: t -> unit [@@js.call]
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

  val length: t -> int [@@js.get]
  val char_code_at: t -> int -> int [@@js.call]
  val to_lower_case: t -> t [@@js.call]
  val to_upper_case: t -> t [@@js.call]
  val concat: t -> (t list [@js.variadic]) -> t [@@js.call]
  val includes: t -> t -> bool [@@js.call]
  val ends_with: t -> t -> bool [@@js.call]
  val index_of: t -> t -> int [@@js.call]
  val repeat: t -> int -> t [@@js.call]
  val search: t -> RegExp.t -> int [@@js.call]
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

module ArrayBuffer : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val create: int -> t [@@js.new "ArrayBuffer"]
end

module Blob : sig
  type options
  val options: ?type_:string -> ?endings:string -> unit -> options [@@js.builder]

  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val create: Ojs.t list -> ?options:options -> unit -> t [@@js.new "Blob"]

  val size: t -> int [@@js.get]
  val type_: t -> string [@@js.get]

  val text: t -> unit -> string Promise.t [@@js.call]
end

module File : sig
  type t = private Blob.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  type options
  val options: ?type_:string -> ?last_modified:float -> unit -> options [@@js.builder]

  val create: Blob.t array -> string -> options -> t [@@js.new "File"]

  val name: t -> string [@@js.get]
end

module DataTransfer : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val files: t -> File.t list [@@js.get]
  val get_data: t -> string -> string [@@js.call]
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

  val target: t -> Ojs.t [@@js.get]
  val related_target: t -> Ojs.t option [@@js.get]
  val prevent_default: t -> unit [@@js.call]
  val stop_propagation: t -> unit [@@js.call]
  val type_: t -> string [@@js.get]

  val init_event: t -> kind -> bool -> bool -> unit [@@js.call]

  val client_x: t -> float (* mouse *) [@@js.get]
  val client_y: t -> float (* mouse *) [@@js.get]

  val page_x: t -> float (* mouse *) [@@js.get]
  val page_y: t -> float (* mouse *) [@@js.get]

  val screen_x: t -> int (* mouse *) [@@js.get]
  val screen_y: t -> int (* mouse *) [@@js.get]

  val movement_x: t -> int (* mouse *) [@@js.get]
  val movement_y: t -> int (* mouse *) [@@js.get]

  val buttons: t -> int (* mouse *) [@@js.get]

  val alt_key: t -> bool (* key *) [@@js.get]
  val ctrl_key: t -> bool (* key *) [@@js.get]
  val shift_key: t -> bool (* key *) [@@js.get]
  val which: t -> int (* key *) [@@js.get]
  val code: t -> string (* key *) [@@js.get]
  val key: t -> string (* key *) [@@js.get]

  val delta_y: t -> float (* wheel *) [@@js.get]
  val delta_x: t -> float (* wheel *) [@@js.get]

  val data_transfer: t -> DataTransfer.t (* drag/drop *) [@@js.get]
  val clipboard_data: t -> DataTransfer.t (* paste *) [@@js.get]

  val data: t -> Ojs.t (* message *) [@@js.get]
  val origin: t -> string (* message *) [@@js.get]

  val state: t -> Ojs.t (* popstate *) [@@js.get]
end

module Rect : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val height: t -> float [@@js.get]
  val width: t -> float [@@js.get]
  val left: t -> float [@@js.get]
  val right: t -> float [@@js.get]
  val top: t -> float [@@js.get]
  val bottom: t -> float [@@js.get]
end

module SVGRect : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val x: t -> float [@@js.get]
  val y: t -> float [@@js.get]
  val height: t -> float [@@js.get]
  val width: t -> float [@@js.get]
end

module Style : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val set: t -> string -> string -> unit
  [@@js.custom
    let set style prop value =
      Ojs.set_prop_ascii (t_to_js style) prop (Ojs.string_to_js value)
    ]
  val set_color: t -> string -> unit [@@js.set]
  val set_border: t -> string -> unit [@@js.set]
  val set_background: t -> string -> unit [@@js.set]
  val set_background_color: t -> string -> unit [@@js.set]
  val set_height: t -> string -> unit [@@js.set]
  val set_width: t -> string -> unit [@@js.set]
  val set_bottom: t -> string -> unit [@@js.set]
  val set_left: t -> string -> unit [@@js.set]
  val set_top: t -> string -> unit [@@js.set]
  val set_right: t -> string -> unit [@@js.set]
  val set_position: t -> string -> unit [@@js.set]
  val set_cursor: t -> string -> unit [@@js.set]
  val set_display: t -> string -> unit [@@js.set]
  val set_visibility: t -> string -> unit [@@js.set]

  val get: t -> string -> string
  [@@js.custom
    let get style prop =
      Ojs.string_of_js (Ojs.get_prop_ascii (t_to_js style) prop)
    ]
  val unset: t -> string -> unit
  [@@js.custom
    let unset style prop =
      Ojs.set_prop_ascii (t_to_js style) prop Ojs.null
    ]
end

module ClassList : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val add: t -> string -> unit [@@js.call]
  val remove: t -> string -> unit [@@js.call]
  val contains: t -> string -> bool [@@js.call]
  val replace: t -> string -> string -> unit [@@js.call]
  val toggle: t -> string -> bool -> bool [@@js.call]
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

  val id: t -> string [@@js.get]
  val set_id: t -> string -> unit [@@js.set]

  type node_type =
    | ELEMENT_NODE [@js 1]
    | TEXT_NODE [@js 3]
    | PROCESSING_INSTRUCTION_NODE [@js 7]
    | COMMENT_NODE [@js 8]
    | DOCUMENT_NODE [@js 9]
    | DOCUMENT_TYPE_NODE [@js 10]
    | DOCUMENT_FRAGMENT_NODE [@js 11]
  [@@js.enum]

  val node_type: t (* T *) -> node_type [@@js.get]

  val clone_node: t (* T *) -> bool -> t [@@js.call]
  val contains: t (* T *) -> t (* T *) -> bool [@@js.call]
  val append_child: t -> t (* T *) -> unit [@@js.call]
  val insert_before: t -> t (* T *) -> t (* T *) -> unit [@@js.call]
  val replace_child: t -> t (* T *) -> t (* T *) -> unit [@@js.call]
  val remove_child: t -> t (* T *) -> unit [@@js.call]
  val first_child: t -> t (* May return Element.null *) [@@js.get]
  val last_child: t -> t (* May return Element.null *) [@@js.get]
  val next_sibling: t (* T *) -> t (* May return Element.null *) [@@js.get]

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
  val add_event_listener: t (* T *) -> Event.kind -> (Event.t -> unit) -> bool -> unit [@@js.call]
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
  val inner_text: t -> string [@@js.get]
  val get_elements_by_tag_name: t -> string -> t array [@@js.call]
  val get_elements_by_class_name: t -> string -> t array [@@js.call]

  val has_attribute: t -> string -> bool [@@js.call]
  val get_attribute: t -> string -> string [@@js.call]
  val remove_attribute: t -> string -> unit [@@js.call]
  val set_attribute: t -> string -> string -> unit [@@js.call]
  val get_bounding_client_rect: t -> Rect.t [@@js.call]
  val get_bounding_box: t (* svg *) -> SVGRect.t [@@js.call "getBBox"]

  type shadow_mode =
    | Open [@js "open"]
    | Closed [@js "closed"]
  [@@js.enum]

  [@@@js.stop]
  val attach_shadow: mode:shadow_mode -> t -> t
  [@@@js.start]
  [@@@js.implem
    type shadow_root_init = { mode: shadow_mode } [@@js]
    val attach_shadow: t -> shadow_root_init -> t [@@js.call]

    let attach_shadow ~mode element = attach_shadow element {mode}
  ]

  val normalize: t (* T *) -> unit [@@js.call]

  val value: t (* <input> *) -> string [@@js.get]
  val set_value: t (* <input> *) -> string -> unit [@@js.set]
  val select: t (* <input> <textarea> *) -> unit [@@js.call]
  val files: t (* <input> *) -> File.t list [@@js.get]
  val submit: t (* <form> *) -> unit [@@js.call]

  val show_picker: t -> unit [@@js.call]

  val selected_index: t (* <select> *) -> int [@@js.get]
  val checked: t (* <input> *) -> bool [@@js.get]
  val set_checked: t (* <input> *) -> bool -> unit [@@js.set]

  val node_value: t (* T *) -> string [@@js.get]
  val set_node_value: t (* T *) -> string -> unit [@@js.set]
  val parent_node: t (* T *) -> t [@@js.get]
  val node_name: t (* T *) -> string [@@js.get]

  val dispatch_event: t (* T *) -> Event.t -> bool [@@js.call]
  val style: t (* T *) -> Style.t [@@js.get]
  val inner_HTML: t -> string [@@js.get]
  val outer_HTML: t -> string [@@js.get]
  val set_inner_HTML: t -> string -> unit [@@js.set]
  val set_text_content: t -> string -> unit [@@js.set]
  val set_class_name: t -> string -> unit [@@js.set]
  val class_name: t -> string [@@js.get]

  val client_width: t -> int [@@js.get]
  val client_height: t -> int [@@js.get]
  val scroll_width: t -> int [@@js.get]
  val scroll_height: t -> int [@@js.get]
  val width: t -> int [@@js.get]
  val height: t -> int [@@js.get]

  val offset_parent: t -> t option [@@js.get]
  val offset_top: t -> int [@@js.get]
  val offset_left: t -> int [@@js.get]
  val offset_width: t -> int [@@js.get]
  val offset_height: t -> int [@@js.get]

  val scroll_top: t -> float [@@js.get]
  val set_scroll_top: t -> float -> unit [@@js.set]
  val scroll_into_view: t -> bool -> unit[@@js.call]
  val focus: t -> unit [@@js.call]
  val blur: t -> unit [@@js.call]

  type scroll_into_view_options = { behavior : behavior option }
  and behavior =
    | Auto [@js "auto"]
    | Instant [@js "instant"]
    | Smooth [@js "smooth"]
  [@@js.enum]

  val scroll_into_view_options: t -> scroll_into_view_options -> unit[@@js.call "scrollIntoView"]

  type scroll_by_options = { top: float; left: float; behavior: behavior option }

  val scroll_by: t -> scroll_by_options -> unit[@@js.call]

  val selection_start: t -> int [@@js.get]
  val selection_end: t -> int [@@js.get]
  val set_selection_start: t -> int -> unit [@@js.set]
  val set_selection_end: t -> int -> unit [@@js.set]

  val remove: t -> unit [@@js.call]
  val click: t -> unit [@@js.call]

  val query_selector: t -> string -> t [@@js.call]
  val query_selector_all: t -> string -> t list [@@js.call]

  val class_list: t -> ClassList.t [@@js.get]
end

module Document: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val create_element: t -> string -> Element.t [@@js.call]
  val create_element_ns: t -> string -> string -> Element.t [@@js.call "createElementNS"]
  val create_text_node: t -> string -> Element.t [@@js.call]
  val create_event: t -> string -> Event.t [@@js.call]

  val get_element_by_id: t -> string -> Element.t option [@@js.call]
  val get_elements_by_class_name: t -> string -> Element.t array [@@js.call]

  val body: t -> Element.t [@@js.get]
  val document_element: t -> Element.t [@@js.get]
  val active_element: t -> Element.t [@@js.get]

  val cookie: t -> string [@@js.get]
  val set_cookie: t -> string -> unit [@@js.set]
  val set_title: t -> string -> unit [@@js.set]

  val open_: t -> ?mime_type:string -> ?history_mode:string -> unit -> unit [@@js.call "open"]
  val write: t -> string -> unit [@@js.call]
  val writeln: t -> string -> unit [@@js.call]
  val close: t -> unit [@@js.call]

  val exec_command: t -> string -> bool [@@js.call]

  val query_selector: t -> string -> Element.t [@@js.call]
  val query_selector_all: t -> string -> Element.t list [@@js.call]

  val remove_all_selection_ranges: t -> unit [@@js.call "getSelection().removeAllRanges"]
end

module History : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val length: t -> int [@@js.get]
  val back: t -> unit [@@js.call]
  val forward: t -> unit [@@js.call]
  val go: t -> ([`Offset of int | `Url of string] [@js.union]) -> unit [@@js.call]

  val replace_state: t -> Ojs.t -> string -> string -> unit [@@js.call]
  val push_state: t -> Ojs.t -> string -> string -> unit [@@js.call]
end

module Location: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val get_hash: unit -> string [@@js.get "location.hash"]
  val set_hash: string -> unit [@@js.set "location.hash"]

  val host: t -> string [@@js.get]
  val set_host: t -> string -> unit [@@js.set]

  val hostname: t -> string [@@js.get]
  val set_hostname: t -> string -> unit [@@js.set]

  val href: unit -> string [@@js.get "location.href"]
  val set_href: string -> unit [@@js.set "location.href"]

  val pathname: t -> string [@@js.get]
  val set_pathname: t -> string -> unit [@@js.set]

  val port: t -> string [@@js.get]
  val set_port: t -> string -> unit [@@js.set]

  val protocol: t -> string [@@js.get]
  val set_protocol: t -> string -> unit [@@js.set]

  val search: t -> string [@@js.get]
  val set_search: t -> string -> unit [@@js.set]

  val origin: t -> string [@@js.get]

  val assign: t -> string -> unit [@@js.call]
  val reload: t -> ?force:bool -> unit -> unit [@@js.call]
  val replace: t -> string -> unit [@@js.call]
end

module Window: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  type timeout_id
  type interval_id

  val add_event_listener: t -> Event.kind -> (Event.t -> unit) -> bool -> unit [@@js.call]
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
  val document: t -> Document.t [@@js.get]
  val set_onload: t -> (unit -> unit) -> unit [@@js.set]
  val set_interval: t -> (unit -> unit) -> int -> interval_id [@@js.call]
  val set_timeout: t -> (unit -> unit) -> int -> timeout_id [@@js.call]
  val clear_timeout: t -> timeout_id -> unit [@@js.call]
  val clear_interval: t -> interval_id -> unit [@@js.call]
  val request_animation_frame: t -> (float -> unit) -> unit [@@js.call]

  val open_: t -> ?url:string -> ?name:string -> ?features:string -> ?replace:bool -> unit -> t [@@js.call]
  val alert: t -> string -> unit [@@js.call]

  val session_storage: t -> Storage.t option [@@js.get]
  val local_storage: t -> Storage.t option [@@js.get]

  val inner_width: t -> float [@@js.get]
  val inner_height: t -> float [@@js.get]
  val page_x_offset: t -> int [@@js.get]
  val page_y_offset: t -> int [@@js.get]
  val scroll_by: t -> int -> int -> unit [@@js.call]
  val scroll_to: t -> int -> int -> unit [@@js.call]

  type scroll_to_options = {
    top: int;
    left: int;
    behavior: Element.behavior option;
  }
  val scroll_to_options: t -> scroll_to_options -> unit [@@js.call "scrollTo"]

  val history: t -> History.t [@@js.get]
  val location : t -> Location.t [@@js.get]

  val frame_element: t -> Element.t [@@js.get]

  val get_computed_style: t -> Element.t -> Style.t [@@js.call]

  val decode_URI_component: t -> string -> string [@@js.call]

  val event_source: Event.t -> t [@@js.get "source"] (* message *)
  val post_message: t -> Ojs.t -> string -> unit [@@js.call]
end

module IFrame: sig
  val content_window: Element.t -> Window.t option [@@js.get]
  val content_document: Element.t -> Document.t option [@@js.get]
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
  val ready_state: t -> state [@@js.get]
  val result: t -> string [@@js.get]
  val set_onload: t -> (unit -> unit) -> unit [@@js.set]
  val read_as_binary_string: t -> File.t -> unit [@@js.call]
  val read_as_text: t -> File.t -> unit [@@js.call]
  val read_as_data_url: t -> File.t -> unit [@@js.call "readAsDataURL"]
end

module XHR: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val create: unit -> t [@@js.new "XMLHttpRequest"]
  val open_: t -> string -> string -> unit [@@js.call]
  val send: t -> Ojs.t -> unit [@@js.call]
  val set_request_header: t -> string -> string -> unit [@@js.call]
  val get_response_header: t -> string -> string option [@@js.call]
  val get_all_response_headers: t -> string [@@js.call]
  val set_response_type: t -> string -> unit [@@js.set]
  val override_mime_type: t -> string -> unit [@@js.call]
  val set_with_credentials: t -> bool -> unit (* starting from IE10 *) [@@js.set]

  type ready_state =
    | Unsent [@js 0]
    | Opened [@js 1]
    | Headers_received [@js 2]
    | Loading [@js 3]
    | Done [@js 4]
    | Other of int [@js.default]
  [@@js.enum]

  val status: t -> int [@@js.get]
  val status_text: t -> string [@@js.get]
  val ready_state: t -> ready_state [@@js.get]
  val response_type: t -> string [@@js.get]
  val response_text: t -> string [@@js.get]
  val response: t -> Ojs.t [@@js.get]
  val response_URL: t -> string [@@js.get]

  val set_onreadystatechange: t -> (unit -> unit) -> unit [@@js.set]
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
  val send : t -> string -> unit [@@js.call]
  val close : t -> ?code:int -> ?reason:string -> unit -> unit [@@js.call]

  val binary_type : t -> string [@@js.get]
  val set_binary_type : t -> string -> unit [@@js.set]
  val ready_state : t -> ready_state [@@js.get]

  val add_event_listener: t -> Event.kind -> (Event.t -> unit) -> bool -> unit [@@js.call]

  module CloseEvent : sig
    type t = Event.t

    val code : t -> int [@@js.get]
  end
end

val window: Window.t [@@js.global]
val document: Document.t [@@js.global]

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
  val set_fill_style: context -> ([`Color of css_color | `Gradient of gradient][@js.union]) -> unit [@@js.set]
  val set_stroke_style: context -> ([`Color of css_color | `Gradient of gradient][@js.union]) -> unit [@@js.set]
  val set_line_width: context -> float -> unit [@@js.set]
  val create_linear_gradient: context -> float -> float -> float -> float -> gradient [@@js.call]
  val add_color_stop: gradient -> float -> css_color -> unit [@@js.call]
  val begin_path: context -> unit [@@js.call]
  val close_path: context -> unit [@@js.call]
  val arc: context -> float -> float -> float -> float -> float -> unit [@@js.call]
  val move_to: context -> float -> float -> unit [@@js.call]
  val line_to: context -> float -> float -> unit [@@js.call]
  val fill: context -> unit [@@js.call]
  val stroke: context -> unit [@@js.call]
  val stroke_rect: context -> float -> float -> float -> float -> unit [@@js.call]
  val fill_rect: context -> float -> float -> float -> float -> unit [@@js.call]
  val set_font: context -> string -> unit [@@js.set]
  val fill_text: context -> string -> float -> float -> unit [@@js.call]
  val stroke_text: context -> string -> float -> float -> unit [@@js.call]
  module TextMetrics : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val width: t -> float [@@js.get]
  end
  val measure_text: context -> string -> TextMetrics.t [@@js.call]
  val rotate: context -> float -> unit [@@js.call]
  val translate: context -> float -> float -> unit [@@js.call]
  val scale: context -> float -> float -> unit [@@js.call]
  val clear_rect: context -> float -> float -> float -> float -> unit [@@js.call]
end

module Performance : sig
  val now: unit -> float
  [@@js.global "performance.now"]
end

module Console : sig
  type t

  val log: t -> Ojs.t -> unit [@@js.call]
  val time: t -> string -> unit [@@js.call]
  val time_end: t -> string -> unit [@@js.call "timeEnd"]
end
val console: Console.t [@@js.global]

module Uint8Array : sig
  type t = private ArrayBuffer.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val from_buffer: ArrayBuffer.t -> t [@@js.new "Uint8Array"]
  val create: int -> t [@@js.new "Uint8Array"]
  val set: t -> int array -> int -> unit [@@js.call]
  val length: t -> int [@@js.get]
  val random: t -> unit [@@js.global "window.crypto.getRandomValues"]
  val to_array: t -> int array [@@js.cast]
  val get: t -> int -> int [@@js.custom let get a i = Ojs.int_of_js (Ojs.array_get (t_to_js a) i)]
  val from: Ojs.t (* arraylike *) -> (Ojs.t -> int) (* map *) -> t [@@js.global "Uint8Array.from"]
end

module ObjectURL : sig
  val of_blob: Blob.t -> string [@@js.global "URL.createObjectURL"]
  val of_file: File.t -> string [@@js.global "URL.createObjectURL"]
  val revoke: string -> unit [@@js.global "URL.revokeObjectURL"]
end

module Svg : sig
  module Length : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val unit_type: t -> int [@@js.get]
    val value: t -> float [@@js.get]
    val value_as_string: t -> string [@@js.get]
    val value_in_specified_units: t -> float [@@js.get]
  end

  module AnimatedLength : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val anim_val: t -> Length.t [@@js.get]
    val base_val: t -> Length.t [@@js.get]
  end
end

module Base64: sig
  val encode: JsString.t -> string [@@js.global "window.btoa"]
  val decode: string -> JsString.t [@@js.global "window.atob"]
end

module FetchResponse: sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val blob: t -> Ojs.t Promise.t [@@js.call]
end

val fetch: string -> FetchResponse.t Promise.t [@@js.global "fetch"]

module TextDecoder: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  (* See encoding list at
     https://developer.mozilla.org/en-US/docs/Web/API/Encoding_API/Encodings *)
  val create: ?label:string -> unit -> t [@@js.new "TextDecoder"]
  val decode: t -> ArrayBuffer.t -> string [@@js.call]
end

module Navigator: sig
  module AuthenticatorAttestationResponse: sig
    type t = Ojs.t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val client_data_json: t -> ArrayBuffer.t [@@js.get "clientDataJSON"]
    val attestation_object: t -> ArrayBuffer.t [@@js.get]
  end

  module AuthenticatorAssertionResponse: sig
    type t = Ojs.t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val client_data_json: t -> ArrayBuffer.t [@@js.get "clientDataJSON"]
    val authenticator_data: t -> ArrayBuffer.t [@@js.get]
    val signature: t -> ArrayBuffer.t [@@js.get]
    val user_handle: t -> ArrayBuffer.t [@@js.get]
  end

  module PublicKeyCredential: sig
    type t = Ojs.t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    val type_: t -> string [@@js.get]
    val id: t -> string [@@js.get]
    val raw_id: t -> ArrayBuffer.t [@@js.get]
    val response: t -> Ojs.t [@@js.get]
  end

  module Credential: sig
    type t = Ojs.t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t

    type rp_options
    val rp_options:
      ?id:string ->
      ?name:string ->
      unit ->
      rp_options [@@js.builder]

    type user_options
    val user_options:
      ?id: ArrayBuffer.t ->
      ?name:string ->
      ?display_name:string ->
      unit ->
      user_options [@@js.builder]

    type key_param
    val key_param:
      ?type_:string ->
      ?alg:int ->
      unit ->
      key_param [@@js.builder]

    type authenticator_options
    val authenticator_options:
      ?authenticator_attachment:string ->
      ?resident_key:string ->
      ?require_resident_key:bool ->
      ?user_verification:string ->
      unit ->
      authenticator_options [@@js.builder]

    type public_key_options
    val public_key_options:
      ?rp:rp_options ->
      ?user:user_options ->
      ?challenge:ArrayBuffer.t ->
      ?pub_key_cred_params:key_param list ->
      ?timeout:int ->
      ?exclude_credentials: Ojs.t list ->
      ?authenticator_selection:authenticator_options ->
      ?attestation:string ->
      unit ->
      public_key_options [@@js.builder]

    type create_options
    val create_options:
      ?public_key:public_key_options ->
      unit ->
      create_options [@@js.builder]

    val create: create_options -> Ojs.t (* Credential *) Promise.t [@@js.global "navigator.credentials.create"]

    type credential_descriptor
    val credential_descriptor:
      ?type_:string ->
      ?id:ArrayBuffer.t ->
      ?transports:Ojs.t ->
      unit ->
      credential_descriptor [@@js.builder]

    type get_public_key_options
    val get_public_key_options:
      ?challenge:ArrayBuffer.t ->
      ?timeout:int ->
      ?rp_id:string ->
      ?allow_credentials:credential_descriptor list ->
      ?user_verification:string ->
      ?extensions:Ojs.t ->
      unit ->
      get_public_key_options [@@js.builder]

    type get_options
    val get_options:
      ?public_key:get_public_key_options ->
      unit ->
      get_options [@@js.builder]

    val get_options_to_js: get_options -> Ojs.t

    val get: get_options -> Ojs.t (* Credential *) Promise.t [@@js.global "navigator.credentials.get"]
  end
end
