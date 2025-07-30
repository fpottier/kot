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

type 'a t = int
let empty = 0
let length    xs = xs
let is_empty  xs = xs = 0
let nonempty  xs = not (is_empty xs)
let singleton  _ = 1
let pop       xs = if xs > 0 then (), xs-1 else assert false
let push    _ xs = xs+1
let eject     xs = if xs > 0 then xs-1, () else assert false
let inject  xs _ = xs+1
let concat xs ys = xs + ys
