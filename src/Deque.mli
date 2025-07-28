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

(**A deque represents a sequence of elements. It is a persistent data
   structure: that is, it is never destroyed or invalidated. In other words,
   it can be thought of as an immutable data structure.

   This data structure can safely be used in concurrent code.

   The time complexity bounds that appear in this documentation are worst-case
   amortized time complexity bounds. It should be noted that these bounds are
   guaranteed to hold only if the data structure is used sequentially. *)
type 'a t

(**[empty] is the empty sequence. *)
val empty : 'a t

(**[length d] is the number of elements in the sequence [d].

   Time complexity: {i O(n)}. *)
val length : 'a t -> int

(**[is_empty d] determines whether the sequence [d] is empty.
   It is equivalent to [length d = 0].

   Time complexity: {i O(1)}. *)
val is_empty : 'a t -> bool

(**[singleton x] returns a singleton sequence (that is, a sequence of
   length 1) whose single element is [x]. *)
val singleton : 'a -> 'a t

(**[push x d] returns a sequence whose elements are the element [x]
   followed with the elements of the sequence [d]. The length of this
   sequence is [1 + length d].

   Time complexity: {i O(1)}. *)
val push : 'a -> 'a t -> 'a t

(**[pop d] returns the sequence [d], deprived of its first element.
   The length of this sequence is [length d - 1]. The sequence [d]
   must be nonempty.

   Time complexity: {i O(1)}. *)
val pop : 'a t -> 'a * 'a t

(**If the sequence [d] is empty then [pop_opt d] is [None].
   If it is nonempty then [pop_opt d] is [Some (pop d)].

   Time complexity: {i O(1)}. *)
val pop_opt : 'a t -> ('a * 'a t) option

(**[inject d x] returns a sequence whose elements are the elements of
   the sequence [d] followed with the element [x]. The length of this
   sequence is [length d + 1].

   Time complexity: {i O(1)}. *)
val inject : 'a t -> 'a -> 'a t

(**[eject d] returns the sequence [d], deprived of its last element.
   The length of this sequence is [length d - 1]. The sequence [d]
   must be nonempty.

   Time complexity: {i O(1)}. *)
val eject : 'a t -> 'a t * 'a

(**If the sequence [d] is empty then [eject_opt d] is [None].
   If it is nonempty then [eject_opt d] is [Some (eject d)].

   Time complexity: {i O(1)}. *)
val eject_opt : 'a t -> ('a t * 'a) option

(**[concat d1 d2] returns a sequence whose elements are the elements of
   the sequence [d1] followed with the elements of the sequence [d2].
   The length of this sequence is [length d1 + length d2].

   Time complexity: {i O(1)}. *)
val concat : 'a t -> 'a t -> 'a t

(**[map f d] is the sequence obtained by applying the transformation
   function [f] to each element of the sequence [d]. The length of this
   sequence is [length d].

   Time complexity: {i O(n)}. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(**[fold_left f s d] is the final state obtained by starting with the
   initial state [s] and by repeatedly applying the transformation function
   [f] to the current state and to each element of the sequence [d] in turn,
   from left to right.

   Time complexity: {i O(n)}. *)
val fold_left : ('s -> 'a -> 's) -> 's -> 'a t -> 's

(**[fold_right f d s] is the final state obtained by starting with the
   initial state [s] and by repeatedly applying the transformation function
   [f] to the current state and to each element of the sequence [d] in turn,
   from right to left.

   Time complexity: {i O(n)}. *)
val fold_right : ('a -> 's -> 's) -> 'a t -> 's -> 's

(**/**)

(*[check d] checks that the internal invariant of the deque [d] holds. *)
val check : 'a t -> unit
