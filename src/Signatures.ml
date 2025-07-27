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

module type BUFFER8 = sig

  include BUFFER

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
