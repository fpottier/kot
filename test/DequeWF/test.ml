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

(* This test uses a trivial reference implementation and focuses on the 6
   essential operations. These features might yield greater performance, that
   is, more test runs per second. Unfortunately, this does not seem to be the
   case. Perhaps the cost of the candidate implementation dominates the cost
   of the reference implementation, so switching to a more efficient reference
   implementation makes no difference? *)

open Monolith

(* This is the reference implementation. *)
(* We use a trivial reference implementation, which computes nothing.
   It is extremely efficient! It is correct only if the deque is used
   to store elements of type [unit]. *)
module R = Testlib.ReferenceTrivial

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

(* We use elements of type [unit]. *)

let element =
  unit

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

(* We test only the 6 essential operations. *)

(* Because we use elements of type [unit], the only failure that we can hope
   to observe is an internal failure of the candidate implementation. *)

let () =

  let spec = deque in
  declare "empty" spec R.empty C.empty;

  let spec = element ^> deque ^> deque in
  declare "push" spec R.push C.push;

  let spec = R.nonempty % deque ^> element *** deque in
  declare "pop" spec R.pop C.pop;

  let spec = deque ^> element ^> deque in
  declare "inject" spec R.inject C.inject;

  let spec = R.nonempty % deque ^> deque *** element in
  declare "eject" spec R.eject C.eject;

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
  let fuel = 256 in
  main ~prologue fuel
