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

val size : 'a buffer -> int

val is_empty : 'a buffer -> bool

val empty : 'a buffer

val push : 'a -> 'a buffer -> 'a buffer

val pop : 'a buffer -> 'a * 'a buffer

val inject : 'a buffer -> 'a -> 'a buffer

val pop2 : 'a buffer -> 'a * 'a * 'a buffer (* requires a full buffer *)

val eject : 'a buffer -> 'a buffer * 'a

val eject2 : 'a buffer -> 'a buffer * 'a * 'a (* requires a full buffer *)
