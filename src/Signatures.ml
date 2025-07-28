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

module type BUFFER = sig

  type 'a buffer

  val empty : 'a buffer

  val length : 'a buffer -> int

  val is_empty : 'a buffer -> bool

  val push : 'a -> 'a buffer -> 'a buffer

  val pop : 'a buffer -> 'a * 'a buffer

  (**[first b] returns the first element of the buffer [b],
     which must be nonempty. It is equivalent to [fst (pop b)]. *)
  val first : 'a buffer -> 'a

  val inject : 'a buffer -> 'a -> 'a buffer

  val eject : 'a buffer -> 'a buffer * 'a

  (**[last b] returns the last element of the buffer [b],
     which must be nonempty. It is equivalent to [snd (eject b)]. *)
  val last : 'a buffer -> 'a

  val map : ('a -> 'b) -> 'a buffer -> 'b buffer

  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a buffer -> 'b

  val fold_right : ('a -> 'b -> 'b) -> 'a buffer -> 'b -> 'b

end

(**The signature [BUFFER8] extends the signature [BUFFER] with extra
   operations. All of these operations are in principle redundant: that is,
   they can be implemented in terms of the basic operations offered by
   [BUFFER]. Nevertheless, it is useful to identify these operations, for two
   reasons: first, their use makes client code more readable; second, a direct
   implementation of these operations is more efficient than an indirect
   implementation in terms of the basic operations. *)
module type BUFFER8 = sig

  include BUFFER

  (**[doubleton x y] constructs a buffer whose length is 2 and whose elements
     are [x] and [y]. *)
  val doubleton : 'a -> 'a -> 'a buffer

  (**[has_length_3 b] is equivalent to [length b = 3]. *)
  val has_length_3 : 'a buffer -> bool

  (**[has_length_6 b] is equivalent to [length b = 6]. *)
  val has_length_6 : 'a buffer -> bool

  (**[has_length_8 b] is equivalent to [length b = 8]. *)
  val has_length_8 : 'a buffer -> bool

  (**[concat32 b1 b2] concatenates the buffers [b1] and [b2]. The buffer [b1]
     must have length 3. The buffer [b2] must have length 2. *)
  val concat32 : 'a buffer -> 'a buffer -> 'a buffer

  (**[concat3x b1 b2] concatenates the buffers [b1] and [b2]. The buffer [b1]
     must have length 3. The buffer [b2] must have length at most 5. *)
  val concat3x : 'a buffer -> 'a buffer -> 'a buffer

  (**[split23l] expects a buffer whose length is comprised between 2 and 5.
     This buffer is split into two buffers [b1] and [b2] such that [b1] has
     length 2 or 3 and [b2] has length 0 or 2 or 3. *)
  val split23l : 'a buffer -> 'a buffer * 'a buffer

  (**[split23r] expects a buffer whose length is comprised between 2 and 5.
     This buffer is split into two buffers [b1] and [b2] such that [b1] has
     length 0 or 2 or 3 and [b2] has length 2 or 3. *)
  val split23r : 'a buffer -> 'a buffer * 'a buffer

  (**[split8] expects a buffer of length 8. This buffer is split into three
     buffers whose lengths are 3, 2, and 3. *)
  val split8 : 'a buffer -> 'a buffer * 'a buffer * 'a buffer

  (***[split642] expects a buffer of length 6. This buffer is split into two
      buffers whose lengths are 4 and 2. *)
  val split642 : 'a buffer -> 'a buffer * 'a buffer

  (***[split624] expects a buffer of length 6. This buffer is split into two
      buffers whose lengths are 2 and 4. *)
  val split624 : 'a buffer -> 'a buffer * 'a buffer

end
