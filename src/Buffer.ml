(******************************************************************************)
(*                                                                            *)
(*                                     Kot                                    *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                         Juliette Ponsonnet, ENS Lyon                       *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

type 'a buffer =
  | B0
  | B1 of 'a
  | B2 of 'a * 'a
  | B3 of 'a * 'a * 'a

let[@inline] size b =
  match b with
  | B0   -> 0
  | B1 _ -> 1
  | B2 _ -> 2
  | B3 _ -> 3

let[@inline] is_empty b =
  match b with
  | B0 -> true
  | _  -> false

(* TODO
let[@inline] is_full b =
  match b with
  | B3 _ -> true
  | _    -> false

let[@inline] singleton x0 =
  B1 x0

let[@inline] doubleton x0 x1 =
  B2 (x0, x1)
 *)

let empty =
  B0

let[@inline] push x0 b =
  match b with
  | B0 ->
      B1 x0
  | B1 x1 ->
      B2 (x0, x1)
  | B2 (x1, x2) ->
      B3 (x0, x1, x2)
  | B3 _ ->
      assert false

let[@inline] pop b =
  match b with
  | B0 ->
      assert false
  | B1 x0 ->
      x0, B0
  | B2 (x0, x1) ->
      x0, B1 x1
  | B3 (x0, x1, x2) ->
      x0, B2 (x1, x2)

let[@inline] pop2 b =
  match b with
  | B3 (x0, x1, x2) ->
      x0, x1, B1 x2
  | _ ->
      assert false

let[@inline] inject b x0 =
  match b with
  | B0 ->
      B1 x0
  | B1 x1 ->
      B2 (x1, x0)
  | B2 (x2, x1) ->
      B3 (x2, x1, x0)
  | B3 _ ->
      assert false

let[@inline] eject2 b =
  match b with
  | B3 (x0, x1, x2) ->
      B1 x0, x1, x2
  | _ ->
      assert false
