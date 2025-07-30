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

type 'a buffer =
  { front: 'a list; back: 'a list }

let empty =
  { front = []; back = [] }

let[@inline] length b =
  List.length b.front + List.length b.back

let[@inline] is_empty b =
  match b.front, b.back with
  | [], [] -> true
  | _,  _  -> false

let[@inline] push x b =
  { b with front = x :: b.front }

let[@inline] pop b =
  match b.front with
  | x :: xs ->
      x, { b with front = xs }
  | [] ->
      match List.rev b.back with
      | x :: xs ->
          x, { front = xs; back = [] }
      | [] ->
          assert false

(* [last xs] is the last element of the list [xs]. *)

let rec last1 x xs =
  match xs with
  | [] ->
      x
  | x :: xs ->
      last1 x xs

let[@inline] last xs =
  match xs with
  | [] ->
      assert false
  | x :: xs ->
      last1 x xs

let[@inline] first b =
  match b.front with
  | x :: _ ->
      x
  | [] ->
      last b.back

let[@inline] inject b x =
  { b with back = x :: b.back }

let[@inline] eject b =
  match b.back with
  | x :: xs ->
      { b with back = xs }, x
  | [] ->
      match List.rev b.front with
      | x :: xs ->
          { back = xs; front = [] }, x
      | [] ->
          assert false

let[@inline] last b =
  match b.back with
  | x :: _ ->
      x
  | [] ->
      last b.front

let[@inline] map f b =
  { front = List.map f b.front; back = List.map f b.back }

(* [fold_left'] is [fold_left], where the two arguments of [f]
   are exchanged, and the last two arguments of [fold_left] are
   exchanged as well. *)

let rec fold_left' f xs accu =
  match xs with
  | [] ->
      accu
  | x :: xs ->
      let accu = f x accu in
      let accu = fold_left' f xs accu in
      accu

(* [fold_right'] is [fold_right], where the two arguments of [f]
   are exchanged, and the last two arguments of [fold_right] are
   exchanged as well. *)

let rec fold_right' f accu xs =
  match xs with
  | [] ->
      accu
  | x :: xs ->
      let accu = fold_right' f accu xs in
      let accu = f accu x in
      accu

let[@inline] fold_left f accu b =
  let accu = List.fold_left f accu b.front in
  let accu = fold_right' f accu b.back in
  accu

let[@inline] fold_right f b accu =
  let accu = fold_left' f b.back accu in
  let accu = List.fold_right f b.front accu in
  accu
