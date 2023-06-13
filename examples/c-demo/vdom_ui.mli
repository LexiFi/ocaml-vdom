(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

(** Reusable VDOM "components" *)

module SelectionList: sig
  open Vdom

  type 'a model
  type msg

  val init: 'a list -> 'a model
  val view: ('a -> string) -> (msg -> 'msg) -> ('a -> 'msg) -> 'a model -> 'msg vdom
  val update: 'a model -> msg -> 'a model
end


module Initializable : sig
  type 'a model
  type ('a, 'msg) msg

  val app:
    init:'a Vdom.Cmd.t ->
    view:('a -> 'msg Vdom.vdom) ->
    update:('a -> 'msg -> 'a * 'msg Vdom.Cmd.t) ->
    unit ->
    ('a model, ('a, 'msg) msg) Vdom.app

  (* Wrap an application that requires an initialization step to get
     its initial state (generated as the outcome of a command). The wrapper
     shows a wait message as long as the initial state is not available. *)
end
