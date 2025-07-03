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

(* Alternative array-based implementation:

type 'a t = 'a array
let empty = [||]
let size = Array.length
let is_empty xs = Array.length xs = 0
let nonempty xs = not (is_empty xs)
let pop     xs = let n = Array.length xs in xs.(0), Array.sub xs 1 (n-1)
let pop_opt xs = let n = Array.length xs in if n > 0 then Some (pop xs) else None
let push x xs = Array.concat [ [|x|]; xs ]

 *)
