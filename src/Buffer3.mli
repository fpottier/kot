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

val empty : 'a buffer

val length : 'a buffer -> int

val is_empty : 'a buffer -> bool

val push : 'a -> 'a buffer -> 'a buffer

val pop : 'a buffer -> 'a * 'a buffer

val inject : 'a buffer -> 'a -> 'a buffer

val eject : 'a buffer -> 'a buffer * 'a

val pop2 : 'a buffer -> 'a * 'a * 'a buffer (* requires a full buffer *)

val eject2 : 'a buffer -> 'a buffer * 'a * 'a (* requires a full buffer *)

val map : ('a -> 'b) -> 'a buffer -> 'b buffer

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a buffer -> 'b
