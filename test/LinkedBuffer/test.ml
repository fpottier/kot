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
module C = Kot.LinkedBuffer
let name = "Kot.LinkedBuffer"

(* -------------------------------------------------------------------------- *)

(* The abstract type [buffer]. *)

let buffer =
  declare_abstract_type ()

(* -------------------------------------------------------------------------- *)

(* We draw random integer elements. *)

let element =
  semi_open_interval 0 32

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let nonfull b =
  R.length b < 8

let () =

  let spec = buffer in
  declare "empty" spec R.empty C.empty;

  let spec = buffer ^> int in
  declare "length" spec R.length C.length;

  let spec = buffer ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = element ^> (nonfull % buffer) ^> buffer in
  declare "push" spec R.push C.push;

  let spec = R.nonempty % buffer ^> element *** buffer in
  declare "pop" spec R.pop C.pop;

  let spec = (nonfull % buffer) ^> element ^> buffer in
  declare "inject" spec R.inject C.inject;

  let spec = R.nonempty % buffer ^> buffer *** element in
  declare "eject" spec R.eject C.eject;

  let spec = R.nonempty % buffer ^> element in
  declare "first" spec R.first C.first;

  let spec = R.nonempty % buffer ^> element in
  declare "last" spec R.last C.last;

  (* [map] and [fold_left] are not tested *)

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    dprintf "          open %s;;\n" name;
    ()
  in
  let fuel = 32 in
  main ~prologue fuel
