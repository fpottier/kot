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

type 'a t = 'a list
let empty = []
let length = List.length
let is_empty xs = match xs with [] -> true | _ :: _ -> false
let nonempty xs = not (is_empty xs)
let singleton x = [x]
let pop     xs = match xs with x :: xs -> x, xs | [] -> assert false
let pop_opt xs = match xs with x :: xs -> Some (x, xs) | [] -> None
let pop2    xs = match xs with x0 :: x1 :: xs -> x0, x1, xs | _ -> assert false
let first = List.hd
let push x xs = x :: xs
let eject  xs = let x, xs = pop (List.rev xs) in List.rev xs, x
let eject_opt xs = match xs with [] -> None | _ -> Some (eject xs)
let last xs = List.hd (List.rev xs) (* slow *)
let inject xs x = xs @ [x]
let concat xs ys = xs @ ys
let fold_left = List.fold_left
let fold_right = List.fold_right
