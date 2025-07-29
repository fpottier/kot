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
  | B4 of 'a * 'a * 'a * 'a
  | B5 of 'a * 'a * 'a * 'a * 'a
  | B6 of 'a * 'a * 'a * 'a * 'a * 'a
  | B7 of 'a * 'a * 'a * 'a * 'a * 'a * 'a
  | B8 of 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

let empty =
  B0

let[@inline] length b =
  match b with
  | B0   -> 0
  | B1 _ -> 1
  | B2 _ -> 2
  | B3 _ -> 3
  | B4 _ -> 4
  | B5 _ -> 5
  | B6 _ -> 6
  | B7 _ -> 7
  | B8 _ -> 8

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
  | B3 (x1, x2, x3) ->
      B4 (x0, x1, x2, x3)
  | B4 (x1, x2, x3, x4) ->
      B5 (x0, x1, x2, x3, x4)
  | B5 (x1, x2, x3, x4, x5) ->
      B6 (x0, x1, x2, x3, x4, x5)
  | B6 (x1, x2, x3, x4, x5, x6) ->
      B7 (x0, x1, x2, x3, x4, x5, x6)
  | B7 (x1, x2, x3, x4, x5, x6, x7) ->
      B8 (x0, x1, x2, x3, x4, x5, x6, x7)
  | B8 _ ->
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
  | B4 (x0, x1, x2, x3) ->
      x0, B3 (x1, x2, x3)
  | B5 (x0, x1, x2, x3, x4) ->
      x0, B4 (x1, x2, x3, x4)
  | B6 (x0, x1, x2, x3, x4, x5) ->
      x0, B5 (x1, x2, x3, x4, x5)
  | B7 (x0, x1, x2, x3, x4, x5, x6) ->
      x0, B6 (x1, x2, x3, x4, x5, x6)
  | B8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
      x0, B7 (x1, x2, x3, x4, x5, x6, x7)

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
  | B4 (x0, _, _, _) ->
      x0
  | B5 (x0, _, _, _, _) ->
      x0
  | B6 (x0, _, _, _, _, _) ->
      x0
  | B7 (x0, _, _, _, _, _, _) ->
      x0
  | B8 (x0, _, _, _, _, _, _, _) ->
      x0

let[@inline] inject b x0 =
  match b with
  | B0 ->
      B1 x0
  | B1 x1 ->
      B2 (x1, x0)
  | B2 (x2, x1) ->
      B3 (x2, x1, x0)
  | B3 (x3, x2, x1) ->
      B4 (x3, x2, x1, x0)
  | B4 (x4, x3, x2, x1) ->
      B5 (x4, x3, x2, x1, x0)
  | B5 (x5, x4, x3, x2, x1) ->
      B6 (x5, x4, x3, x2, x1, x0)
  | B6 (x6, x5, x4, x3, x2, x1) ->
      B7 (x6, x5, x4, x3, x2, x1, x0)
  | B7 (x7, x6, x5, x4, x3, x2, x1) ->
      B8 (x7, x6, x5, x4, x3, x2, x1, x0)
  | B8 _ ->
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
  | B4 (x0, x1, x2, x3) ->
      B3 (x0, x1, x2), x3
  | B5 (x0, x1, x2, x3, x4) ->
      B4 (x0, x1, x2, x3), x4
  | B6 (x0, x1, x2, x3, x4, x5) ->
      B5 (x0, x1, x2, x3, x4), x5
  | B7 (x0, x1, x2, x3, x4, x5, x6) ->
      B6 (x0, x1, x2, x3, x4, x5), x6
  | B8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
      B7 (x0, x1, x2, x3, x4, x5, x6), x7

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
  | B4 (_, _, _, x3) ->
      x3
  | B5 (_, _, _, _, x4) ->
      x4
  | B6 (_, _, _, _, _, x5) ->
      x5
  | B7 (_, _, _, _, _, _, x6) ->
      x6
  | B8 (_, _, _, _, _, _, _, x7) ->
      x7

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
  | B4 (x0, x1, x2, x3) ->
      B4 (f x0, f x1, f x2, f x3)
  | B5 (x0, x1, x2, x3, x4) ->
      B5 (f x0, f x1, f x2, f x3, f x4)
  | B6 (x0, x1, x2, x3, x4, x5) ->
      B6 (f x0, f x1, f x2, f x3, f x4, f x5)
  | B7 (x0, x1, x2, x3, x4, x5, x6) ->
      B7 (f x0, f x1, f x2, f x3, f x4, f x5, f x6)
  | B8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
      B8 (f x0, f x1, f x2, f x3, f x4, f x5, f x6, f x7)

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
  | B4 (x0, x1, x2, x3) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      let accu = f accu x2 in
      let accu = f accu x3 in
      accu
  | B5 (x0, x1, x2, x3, x4) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      let accu = f accu x2 in
      let accu = f accu x3 in
      let accu = f accu x4 in
      accu
  | B6 (x0, x1, x2, x3, x4, x5) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      let accu = f accu x2 in
      let accu = f accu x3 in
      let accu = f accu x4 in
      let accu = f accu x5 in
      accu
  | B7 (x0, x1, x2, x3, x4, x5, x6) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      let accu = f accu x2 in
      let accu = f accu x3 in
      let accu = f accu x4 in
      let accu = f accu x5 in
      let accu = f accu x6 in
      accu
  | B8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
      let accu = f accu x0 in
      let accu = f accu x1 in
      let accu = f accu x2 in
      let accu = f accu x3 in
      let accu = f accu x4 in
      let accu = f accu x5 in
      let accu = f accu x6 in
      let accu = f accu x7 in
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
  | B4 (x0, x1, x2, x3) ->
      let accu = f x3 accu in
      let accu = f x2 accu in
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu
  | B5 (x0, x1, x2, x3, x4) ->
      let accu = f x4 accu in
      let accu = f x3 accu in
      let accu = f x2 accu in
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu
  | B6 (x0, x1, x2, x3, x4, x5) ->
      let accu = f x5 accu in
      let accu = f x4 accu in
      let accu = f x3 accu in
      let accu = f x2 accu in
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu
  | B7 (x0, x1, x2, x3, x4, x5, x6) ->
      let accu = f x6 accu in
      let accu = f x5 accu in
      let accu = f x4 accu in
      let accu = f x3 accu in
      let accu = f x2 accu in
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu
  | B8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
      let accu = f x7 accu in
      let accu = f x6 accu in
      let accu = f x5 accu in
      let accu = f x4 accu in
      let accu = f x3 accu in
      let accu = f x2 accu in
      let accu = f x1 accu in
      let accu = f x0 accu in
      accu

let doubleton x y =
  B2 (x, y)

let decompose_doubleton b =
  match b with
  | B2 (x, y) ->
      x, y
  | _ ->
      assert false

let has_length_3 b =
  match b with
  | B3 _ -> true
  | _    -> false

let has_length_6 b =
  match b with
  | B6 _ -> true
  | _    -> false

let has_length_8 b =
  match b with
  | B8 _ -> true
  | _    -> false

let move_left_1_33 b1 b2 =
  match b1, b2 with
  | B3 (x0, x1, x2), B3 (x3, x4, x5) ->
      B4 (x0, x1, x2, x3), B2 (x4, x5)
  | _, _ ->
      assert false

let concat32 b1 b2 =
  match b1, b2 with
  | B3 (x0, x1, x2), B2 (x3, x4) ->
      B5 (x0, x1, x2, x3, x4)
  | _, _ ->
      assert false

let concat323 b1 b2 b3 =
  match b1, b2, b3 with
  | B3 (x0, x1, x2), B2 (x3, x4), B3 (x5, x6, x7) ->
      B8 (x0, x1, x2, x3, x4, x5, x6, x7)
  | _, _, _ ->
      assert false

let concat3x b1 b2 =
  match b1 with
  | B3 (x0, x1, x2) ->
      begin match b2 with
      | B0 ->
          b1
      | B1 x3 ->
          B4 (x0, x1, x2, x3)
      | B2 (x3, x4) ->
          B5 (x0, x1, x2, x3, x4)
      | B3 (x3, x4, x5) ->
          B6 (x0, x1, x2, x3, x4, x5)
      | B4 (x3, x4, x5, x6) ->
          B7 (x0, x1, x2, x3, x4, x5, x6)
      | B5 (x3, x4, x5, x6, x7) ->
          B8 (x0, x1, x2, x3, x4, x5, x6, x7)
      | B6 _
      | B7 _
      | B8 _ ->
          assert false
      end
  | _ ->
      assert false

let concatx3 b1 b2 =
  match b2 with
  | B3 (x2, x1, x0) ->
      begin match b1 with
      | B0 ->
          b2
      | B1 x3 ->
          B4 (x3, x2, x1, x0)
      | B2 (x4, x3) ->
          B5 (x4, x3, x2, x1, x0)
      | B3 (x5, x4, x3) ->
          B6 (x5, x4, x3, x2, x1, x0)
      | B4 (x6, x5, x4, x3) ->
          B7 (x6, x5, x4, x3, x2, x1, x0)
      | B5 (x7, x6, x5, x4, x3) ->
          B8 (x7, x6, x5, x4, x3, x2, x1, x0)
      | B6 _
      | B7 _
      | B8 _ ->
          assert false
      end
  | _ ->
      assert false

let split23l b =
  match b with
  | B2 _
  | B3 _ ->
      b, empty
  | B4 (x0, x1, x2, x3) ->
      B2 (x0, x1), B2 (x2, x3)
  | B5 (x0, x1, x2, x3, x4) ->
      B2 (x0, x1), B3 (x2, x3, x4)
  | _ ->
      assert false

let split23r b =
  match b with
  | B2 _
  | B3 _ ->
      empty, b
  | B4 (x0, x1, x2, x3) ->
      B2 (x0, x1), B2 (x2, x3)
  | B5 (x0, x1, x2, x3, x4) ->
      B2 (x0, x1), B3 (x2, x3, x4)
  | _ ->
      assert false

let split8 b =
  match b with
  | B8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
      B3 (x0, x1, x2), B2 (x3, x4), B3 (x5, x6, x7)
  | _ ->
      assert false

let split642 b =
  match b with
  | B6 (x0, x1, x2, x3, x4, x5) ->
      B4 (x0, x1, x2, x3), B2 (x4, x5)
  | _ ->
      assert false

let split624 b =
  match b with
  | B6 (x0, x1, x2, x3, x4, x5) ->
      B2 (x0, x1), B4 (x2, x3, x4, x5)
  | _ ->
      assert false
