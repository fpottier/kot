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

(*
type 'a catdeque

val empty : 'a catdeque

val size : 'a catdeque -> int

val is_empty : 'a catdeque -> bool

val push : 'a -> 'a catdeque -> 'a catdeque

val pop : 'a catdeque -> 'a * 'a catdeque

val inject : 'a catdeque -> 'a -> 'a catdeque

val eject : 'a catdeque -> 'a catdeque * 'a

val pop_opt : 'a catdeque -> ('a * 'a catdeque) option

val eject_opt : 'a catdeque -> ('a catdeque * 'a) option

val check : 'a catdeque -> unit

val concat : 'a catdeque -> 'a catdeque -> 'a catdeque
*)
