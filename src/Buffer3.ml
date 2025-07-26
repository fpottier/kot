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

type 'a buffer =
  | B0
  | B1 of 'a
  | B2 of 'a * 'a
  | B3 of 'a * 'a * 'a

let empty =
  B0

let[@inline] length b =
  match b with
  | B0   -> 0
  | B1 _ -> 1
  | B2 _ -> 2
  | B3 _ -> 3

let[@inline] is_empty b =
  match b with
  | B0 -> true
  | _  -> false

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

let[@inline] first b =
  match b with
  | B0 ->
      assert false
  | B1 x0 ->
      x0
  | B2 (x0, _) ->
      x0
  | B3 (x0, _, _) ->
      x0

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

let[@inline] eject b =
  match b with
  | B0 ->
      assert false
  | B1 (x0) ->
      B0, x0
  | B2 (x0, x1) ->
      B1 (x0), x1
  | B3 (x0, x1, x2) ->
      B2 (x0, x1), x2

let[@inline] last b =
  match b with
  | B0 ->
      assert false
  | B1 (x0) ->
      x0
  | B2 (_, x1) ->
      x1
  | B3 (_, _, x2) ->
      x2

let[@inline] pop2 b =
  match b with
  | B3 (x0, x1, x2) ->
      x0, x1, B1 x2
  | _ ->
      assert false

let[@inline] eject2 b =
  match b with
  | B3 (x0, x1, x2) ->
      B1 x0, x1, x2
  | _ ->
      assert false

let[@inline] map f b =
  match b with
  | B0 ->
      B0
  | B1 (x0) ->
      B1 (f x0)
  | B2 (x0, x1) ->
      B2 (f x0, f x1)
  | B3 (x0, x1, x2) ->
      B3 (f x0, f x1, f x2)

let[@inline] fold_left f accu b =
  match b with
  | B0 ->
      accu
  | B1 (x0) ->
      let accu = f accu x0 in
      accu
  | B2 (x0, x1) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      accu
  | B3 (x0, x1, x2) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      let accu = f accu x2 in
      accu

let[@inline] fold_right f b accu =
  match b with
  | B0 ->
      accu
  | B1 (x0) ->
      let accu = f x0 accu in
      accu
  | B2 (x0, x1) ->
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu
  | B3 (x0, x1, x2) ->
      let accu = f x2 accu in
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu
