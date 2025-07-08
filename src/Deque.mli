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

type 'a deque

val empty : 'a deque

(**[length d] is the number of elements in the sequence [d]. *)
val length : 'a t -> int

val is_empty : 'a deque -> bool

val push : 'a -> 'a deque -> 'a deque

val pop : 'a deque -> 'a * 'a deque

val inject : 'a deque -> 'a -> 'a deque

val eject : 'a deque -> 'a deque * 'a

val pop_opt : 'a deque -> ('a * 'a deque) option

val eject_opt : 'a deque -> ('a deque * 'a) option

val concat : 'a deque -> 'a deque -> 'a deque

val check : 'a deque -> unit

val map : ('a -> 'b) -> 'a deque -> 'b deque

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a deque -> 'b
