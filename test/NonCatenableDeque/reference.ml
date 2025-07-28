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

(* Alternative array-based implementation:

type 'a t = 'a array
let empty = [||]
let length = Array.length
let is_empty xs = Array.length xs = 0
let nonempty xs = not (is_empty xs)
let pop     xs = let n = Array.length xs in xs.(0), Array.sub xs 1 (n-1)
let pop_opt xs = let n = Array.length xs in if n > 0 then Some (pop xs) else None
let push x xs = Array.concat [ [|x|]; xs ]

 *)
