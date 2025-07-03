(******************************************************************************)
(*                                                                            *)
(*                                  Kot                                  *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* This is the candidate implementation. *)
module C = Kot.Buffer3
let name = "Kot.Buffer3"

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
  R.size b < 3

let () =

  let spec = buffer in
  declare "empty" spec R.empty C.empty;

  let spec = buffer ^> int in
  declare "size" spec R.size C.size;

  let spec = buffer ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = element ^> (nonfull % buffer) ^> buffer in
  declare "push" spec R.push C.push;

  let spec = R.nonempty % buffer ^> element *** buffer in
  declare "pop" spec R.pop C.pop;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    dprintf "          open %s;;\n" name;
    ()
  in
  let fuel = 16 in
  main ~prologue fuel
