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

type 'a t = 'a array
let empty = [||]
let length = Array.length
let is_empty  xs = Array.length xs = 0
let nonempty  xs = not (is_empty xs)
let singleton  x = [|x|]
let pop       xs = let n = Array.length xs in xs.(0), Array.sub xs 1 (n-1)
let pop_opt   xs = let n = Array.length xs in if n > 0 then Some (pop xs) else None
let pop2      xs = let n = Array.length xs in xs.(0), xs.(1), Array.sub xs 2 (n-2)
let first     xs = xs.(0)
let push    x xs = Array.concat [ [|x|]; xs ]
let eject     xs = let n = Array.length xs in Array.sub xs 0 (n-1), xs.(n-1)
let eject_opt xs = let n = Array.length xs in if n > 0 then Some (eject xs) else None
let last      xs = let n = Array.length xs in xs.(n-1)
let inject  xs x = Array.concat [ xs; [|x|] ]
let concat xs ys = Array.concat [ xs; ys ]
let fold_left = Array.fold_left
let fold_right = Array.fold_right
