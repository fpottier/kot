(******************************************************************************)
(*                                                                            *)
(*                                     Kot                                    *)
(*                                                                            *)
(*                         Juliette Ponsonnet, ENS Lyon                       *)
(*                         François Pottier, Inria Paris                      *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* This is a variant of the test in Deque. In this variant, we focus on the
   most essential operations. Furthermore, we introduce "batch" operations
   where one batch operation stands for a sequence of N atomic operations.
   This allows us to build very large data structures while consuming only
   a small amount of Monolith fuel. *)

open Monolith

(* This is the reference implementation. *)
module R = struct
  include Testlib.Reference
  let rec batch_push n x xs =
    if n = 0 then xs else
    let xs = push x xs in
    batch_push (n-1) x xs
  let rec batch_inject n xs x =
    if n = 0 then xs else
    let xs = inject xs x in
    batch_inject (n-1) xs x
  let rec batch_pop xs n =
    if n = 1 then pop xs
    else let _, xs = pop xs in
    batch_pop xs (n-1)
  let rec batch_eject xs n =
    if n = 1 then eject xs
    else let xs, _ = eject xs in
    batch_eject xs (n-1)
end

(* This is the candidate implementation. *)
module C = struct
  include Kot.Deque
  let rec batch_push n x xs =
    if n = 0 then xs else
    let xs = push x xs in
    batch_push (n-1) x xs
  let rec batch_inject n xs x =
    if n = 0 then xs else
    let xs = inject xs x in
    batch_inject (n-1) xs x
  let rec batch_pop xs n =
    if n = 1 then pop xs
    else let _, xs = pop xs in
    batch_pop xs (n-1)
  let rec batch_eject xs n =
    if n = 1 then eject xs
    else let xs, _ = eject xs in
    batch_eject xs (n-1)
end

let () =
  dprintf "          open %s;;\n" "Kot.Deque";
  dprintf
{|          let rec batch_push n x xs =
            if n = 0 then xs else
            let xs = push x xs in
            batch_push (n-1) x xs;;
          let rec batch_inject n xs x =
            if n = 0 then xs else
            let xs = inject xs x in
            batch_inject (n-1) xs x;;
          let rec batch_pop xs n =
            if n = 1 then pop xs
            else let _, xs = pop xs in
            batch_pop xs (n-1);;
          let rec batch_eject xs n =
            if n = 1 then eject xs
            else let xs, _ = eject xs in
            batch_eject xs (n-1);;
|}

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

(* The size of a batch [push] or batch [inject]. *)

let size =
  semi_open_interval 1 32

(* The size of a batch [pop] or batch [eject]
   is at most the length of the deque [d]. *)

let range d =
  closed_interval 1 (R.length d)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = deque in
  declare "empty" spec R.empty C.empty;

  let spec = size ^> element ^> deque ^> deque in
  declare "batch_push" spec R.batch_push C.batch_push;

  let spec = size ^> deque ^> element ^> deque in
  declare "batch_inject" spec R.batch_inject C.batch_inject;

  let spec = deque ^>> fun d -> range d ^> element *** deque in
  declare "batch_pop" spec R.batch_pop C.batch_pop;

  let spec = deque ^>> fun d -> range d ^> deque *** element in
  declare "batch_eject" spec R.batch_eject C.batch_eject;

  let spec = deque ^> deque ^> deque in
  declare "concat" spec R.concat C.concat;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 48 in
  main fuel
