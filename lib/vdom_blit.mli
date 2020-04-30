(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)


(** {1 Rendering "Virtual applications" to concrete ones} *)


module Cmd: sig
  type 'msg ctx
  val send_msg: 'msg ctx -> 'msg -> unit

  type handler = {f: 'msg. 'msg ctx -> 'msg Vdom.Cmd.t -> bool}
  (** A command handler recognizes some commands and reacts on them,
      typically by spawing asynchronous tasks than can notify their
      outcome with [send_msg]. *)
end

module Custom: sig
  type t
  (** A controller for a custom element. *)

  val make: ?dispose:(unit -> unit) -> sync:(Vdom.Custom.t -> bool) -> Js_browser.Element.t -> t
  (** Create a custom controller out of DOM element.

      The [sync] function is in charge of updating the internal state
      of the custom element when the VDOM changes.  It must return [true]
      if the update was successful, or [false] if the element needs to be destroyed
      and recreated. *)


  type ctx
  (** Context for custom element handlers. *)

  val parent: ctx -> Js_browser.Element.t

  val send_event: ctx -> Vdom.event -> unit
  (** Can only be called after the handler returns (typically in a DOM event callback). *)

  val after_redraw: ctx -> (unit -> unit) -> unit
  (** Record an action to be executed after the next redraw. *)

  type handler = ctx -> Vdom.Custom.t -> t option
  (** A custom element handler recognizes some kinds of custom elements
      described in the VDOM and instantiante a concrete controller for them. *)
end


(** {2 Extension hooks (command and custom element handlers)} *)

type env
val cmd: Cmd.handler -> env
val custom: Custom.handler -> env
val merge: env list -> env
val empty: env

val register: env -> unit
(** Register global rules (command and custom element handlers).
    Local ones can also be passed explicitly to [run]. *)

(** {2 Application controller} *)

type ('model, 'msg) app

val run:
  ?env:env ->
  ?container:Js_browser.Element.t ->
  ('model, 'msg) Vdom.app ->
  ('model, 'msg) app
(** Instantion a VDOM application into a concrete one, running
    into a fixed fresh DOM container node. *)

val dispose: ('model, 'msg) app -> unit
(** Dispose all the resources attached to an application.
    If the container was provided on run, it is emptied on disposal, otherwise it is removed from the DOM. *)

val dom: ('model, 'msg) app -> Js_browser.Element.t
(** Returns the main DOM node that serves as the container for a Vdom
    application. *)

val process: ('model, 'msg) app -> 'msg -> unit
(** Inject a message into a VDOM application. *)

val get: ('model, 'msg) app -> 'model
(** Get the current model of the VDOM application. *)

val after_redraw: ('model, 'msg) app -> (unit -> unit) -> unit
(** Execute the callback after the next redraw *)
