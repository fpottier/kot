(******************************************************************************)
(*                                                                            *)
(*                                     Kot                                    *)
(*                                                                            *)
(*                         François Pottier, Inria Paris                      *)
(*                         Juliette Ponsonnet, ENS Lyon                       *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* This is the candidate implementation. *)
module C = Kot.Deque
let name = "Kot.Deque"

(* -------------------------------------------------------------------------- *)

(* The abstract type [deque]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check (_model : _ R.t) =
  C.check,
  constant "check"

let deque =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* We draw random integer elements. *)

let element =
  semi_open_interval 0 32

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = deque in
  declare "empty" spec R.empty C.empty;

  let spec = deque ^> int in
  declare "length" spec R.length C.length;

  let spec = deque ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = element ^> deque in
  declare "singleton" spec R.singleton C.singleton;

  let spec = element ^> deque ^> deque in
  declare "push" spec R.push C.push;

  let spec = R.nonempty % deque ^> element *** deque in
  declare "pop" spec R.pop C.pop;

  let spec = deque ^> option (element *** deque) in
  declare "pop_opt" spec R.pop_opt C.pop_opt;

  let spec = deque ^> element ^> deque in
  declare "inject" spec R.inject C.inject;

  let spec = R.nonempty % deque ^> deque *** element in
  declare "eject" spec R.eject C.eject;

  let spec = deque ^> option (deque *** element) in
  declare "eject_opt" spec R.eject_opt C.eject_opt;

  let spec = deque ^> deque ^> deque in
  declare "concat" spec R.concat C.concat;


  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    dprintf "          open %s;;\n" name;
    ()
  in
  let fuel = 128 in
  main ~prologue fuel
