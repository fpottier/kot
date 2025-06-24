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

type 'a t = 'a list
let empty = []
let size = List.length
let is_empty xs = match xs with [] -> true | _ :: _ -> false
let nonempty xs = not (is_empty xs)
let pop     xs = match xs with x :: xs -> x, xs | [] -> assert false
let pop_opt xs = match xs with x :: xs -> Some (x, xs) | [] -> None
let push x xs = x :: xs
let eject  xs = match List.rev xs with x::xs -> List.rev xs, x  | [] -> assert false
let eject_opt xs = match List.rev xs with x :: xs -> Some (List.rev xs, x) | [] -> None
let inject xs x = xs @ [x]
