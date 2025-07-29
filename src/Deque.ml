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

(* -------------------------------------------------------------------------- *)

(* As a building block, we need buffers (that is, deques) of capacity 8. *)

module B = struct

  include Buffer8

  (* [iter] is used only by [check], so its efficiency is not critical. *)
  let iter (type a) (f : a -> unit) (b : a buffer) : unit =
    fold_left (fun () x -> f x) () b

  (* [length_is_between] is also used only by [check]. *)
  let length_is_between b min max =
    min <= length b && length b <= max

end

type 'a buffer =
  'a B.buffer

(* -------------------------------------------------------------------------- *)

(* The data structure. *)

(* The reference is a stable reference: every update of this reference
   overwrites a previous 5-tuple with a new 5-tuple that represents the
   same sequence of elements. Thus, even though this data structure
   involves mutable state, it appears immutable to the user. *)

type 'a deque =
  'a nonempty_deque option

and 'a nonempty_deque =
  'a five_tuple ref

and 'a five_tuple = {
  prefix : 'a buffer;
  left   : 'a triple deque;
  middle : 'a buffer;
  right  : 'a triple deque;
  suffix : 'a buffer;
}

and 'a triple = {
  first  : 'a buffer;
  child  : 'a triple deque;
  last   : 'a buffer;
}

type 'a t =
  'a deque

(* -------------------------------------------------------------------------- *)

(* The data structure obeys the following invariant. *)

let rec check_deque : type a. (a -> unit) -> a deque -> unit = fun check_elem d ->
  match d with
  | None ->
      ()
  | Some r ->
      let { prefix; left; middle; right; suffix } = !r in
      (* Check each component of this 5-tuple. *)
      B.iter check_elem prefix;
      check_deque (check_triple check_elem) left;
      B.iter check_elem middle;
      check_deque (check_triple check_elem) right;
      B.iter check_elem suffix;
      (* Check the length constraints at this level. *)
      if B.is_empty middle then begin
        (* A 5-tuple whose middle buffer is empty is "suffix-only". *)
        assert (B.length_is_between prefix 0 0);
        assert (left = None);
        assert (right = None);
        assert (B.length_is_between suffix 1 8)
      end
      else begin
        assert (B.length_is_between prefix 3 6);
        assert (B.length middle = 2);
        assert (B.length_is_between suffix 3 6);
      end

and check_triple : type a. (a -> unit) -> a triple -> unit = fun check_elem t ->
  let { first; child; last } = t in
  (* Check each component of this triple. *)
  B.iter check_elem first;
  check_deque (check_triple check_elem) child;
  B.iter check_elem last;
  (* Check the length constraints at this level. *)
  check_triple_local t

(* Inside a triple, we say that the buffer [first] or [last] is ordinary
   if its size is 2 or 3. If it is not ordinary then it must be empty. *)

and is_ordinary : type a. a buffer -> bool = fun b ->
  B.length_is_between b 2 3

and check_triple_local : type a. a triple -> unit = fun t ->
  let { first; child; last } = t in
  (* The buffer [first] is always ordinary. In the paper, this invariant
     is not imposed when a triple is constructed; instead, it is imposed
     in some places when a triple is inspected (e.g., in cases 1 and 2
     in the description of [pop]). *)
  assert (is_ordinary first);
  (* When [child] is empty, [last] is either ordinary or empty.
     When [child] is nonempty, [last] is ordinary. *)
  match child with
  | None ->
      assert (is_ordinary last || B.is_empty last)
  | Some _ ->
      assert (is_ordinary last)

let check d =
  check_deque (fun _x -> ()) d

(* -------------------------------------------------------------------------- *)

(* The empty deque is represented by [None], and only by [None]. *)

let empty =
  None

let is_empty =
  Option.is_none

(* A singleton deque is constructed as follows. *)

let singleton x =
  Some (ref {
    prefix = B.empty;
    left = empty;
    middle = B.empty;
    right = empty;
    suffix = B.push x B.empty
  })

(* -------------------------------------------------------------------------- *)

(* [assemble_] and [assemble] construct a deque whose content is the 5-tuple
   [{ prefix; left; middle; right; suffix }].

   [assemble_] does not allow [middle] and [suffix] to be both empty.
   It allocates a new 5-tuple and a new deque.

   [assemble] allows this situation. In this case, this 5-tuple must be
   entirely empty; then, the empty deque is returned. *)

let[@inline] assemble_ prefix left middle right suffix : _ deque =
  assert (not (B.is_empty middle && B.is_empty suffix));
  Some (ref { prefix; left; middle; right; suffix })

let[@inline] assemble prefix left middle right suffix : _ deque =
  if B.is_empty middle && B.is_empty suffix then
    empty
  else
    assemble_ prefix left middle right suffix

(* [triple] constructs the triple [{ first; child; last }]. *)

let[@inline] triple first child last : _ triple =
  let t = { first; child; last } in
  assert (check_triple_local t; true);
  t

(* [buffer] converts a nonempty buffer into a triple. *)

let[@inline] buffer (type a) (b : a buffer) : a triple =
  assert (not (B.is_empty b));
  triple b empty B.empty

(* -------------------------------------------------------------------------- *)

(* Insertion at the front end. *)

let rec push : type a. a -> a deque -> a deque = fun x c ->
  match c with
  | None ->
      singleton x
  | Some r ->
      let f = !r in
      let { prefix; left; middle; right; suffix } = f in
      if B.is_empty middle then
        if B.has_length_8 suffix then
          let prefix, middle, suffix = B.split8 suffix in
          r := { f with prefix; middle; suffix };
          assemble_ (B.push x prefix) left middle right suffix
        else
          assemble_ B.empty empty B.empty empty (B.push x suffix)
      else
        if B.has_length_6 prefix then
          let prefix, prefix' = B.split642 prefix in
          let left = push (buffer prefix') left in
          r := { f with prefix; left };
          assemble_ (B.push x prefix) left middle right suffix
        else
          assemble_ (B.push x prefix) left middle right suffix

(* Insertion at the rear end. *)

let rec inject : type a. a deque -> a -> a deque = fun c x ->
  match c with
  | None ->
      singleton x
  | Some r ->
      let f = !r in
      let { prefix; left; middle; right; suffix } = f in
      if B.is_empty middle then
        if B.has_length_8 suffix then
          let prefix, middle, suffix = B.split8 suffix in
          r := { f with prefix; middle; suffix };
          assemble_ prefix left middle right (B.inject suffix x)
        else
          assemble_ B.empty empty B.empty empty (B.inject suffix x)
      else
        if B.has_length_6 suffix then
          let suffix', suffix = B.split624 suffix in
          let right = inject right (buffer suffix') in
          r := { f with right; suffix };
          assemble_ prefix left middle right (B.inject suffix x)
        else
          assemble_ prefix left middle right (B.inject suffix x)

(* -------------------------------------------------------------------------- *)

(* Concatenation. *)

let concat d1 d2 =
  match d1, d2 with
  | None, _ ->
      d2
  | _, None ->
      d1
  | Some r1, Some r2 ->
      let { prefix = prefix1; left = left1; middle = middle1;
                            right = right1; suffix = suffix1 } = !r1
      and { prefix = prefix2; left = left2; middle = middle2;
                            right = right2; suffix = suffix2 } = !r2 in
      match B.is_empty middle1, B.is_empty middle2 with
      | false, false ->
          (* Extract the last element [d1] and the first element of [d2].
             Form a middle buffer with them. *)
          let suffix1, x1 = B.eject suffix1
          and x2, prefix2 = B.pop prefix2 in
          let middle = B.doubleton x1 x2 in
          (* The length of [suffix1] is between 2 and 5. Split it into two
             chunks of length 2 or 3 -- except the second chunk may have
             length 0. *)
          let suffix1a, suffix1b = B.split23l suffix1 in
          (* Inject [middle1], [right1], and [suffix1a], as a triple,
             into [left1]. *)
          let left1 = inject left1 (triple middle1 right1 suffix1a) in
          (* Unless it is empty, inject [suffix1b], as a triple,
             into [left1]. *)
          let left1 =
            if B.is_empty suffix1b then left1
            else inject left1 (triple suffix1b empty B.empty)
          in
          (* The length of [prefix2] is between 2 and 5. Split it into two
             chunks of length 2 or 3 -- except the first chunk may have
             length 0. *)
          let prefix2a, prefix2b = B.split23r prefix2 in
          (* Push [prefix2b], [left2], and [middle2], as a triple,
             into [right2]. *)
          let right2 = push (triple prefix2b left2 middle2) right2 in
          (* Unless it is empty, push [prefix2a], as a triple,
             into [right2]. *)
          let right2 =
            if B.is_empty prefix2a then right2
            else push (triple prefix2a empty B.empty) right2
          in
          (* Done. *)
          assemble_ prefix1 left1 middle right2 suffix2
      | _, true ->
          (* [d2] is suffix-only. *)
          B.fold_left inject d1 suffix2
      | true, _ ->
          (* [d1] is suffix-only. *)
          B.fold_right push suffix1 d2

(* -------------------------------------------------------------------------- *)

(* Extraction at the front end. *)

(* [naive_pop_safe f] is a sufficient condition for [naive_pop f]
   to produce a valid deque. (See below.) *)

let[@inline] naive_pop_safe (f : 'a five_tuple) : bool =
  let { prefix; middle; _ } = f in
  B.is_empty middle || B.length prefix > 3

(* [naive_pop] expects a 5-tuple [f], extracts its first element [x],
   and returns the remaining elements as a deque [d]. *)

(* There are two distinct scenarios under which [naive_pop] is used.

   In the first scenario, the precondition [naive_pop_safe f] holds.
   Then, [naive_pop f] returns a valid deque.

   In the second scenario, [naive_pop_safe f] does not necessarily
   hold. [naive_pop f] returns a pair [(x, d)] where the deque [d]
   is not valid. Then, for every [x'], the operation [push x' d]
   will produce a valid deque again. *)
(* TODO this remains to be confirmed by the proof *)

let naive_pop (type a) (f : a five_tuple) : a * a deque =
  let { prefix; left; middle; right; suffix } = f in
  if B.is_empty middle then
    let x, suffix = B.pop suffix in
    x, assemble prefix left middle right suffix
  else
    let x, prefix = B.pop prefix in
    x, assemble_ prefix left middle right suffix

let inspect_first (type a) (f : a five_tuple) : a =
  let { prefix; left; middle; right; suffix } = f in
  if B.is_empty middle then
    (* This 5-tuple is suffix-only. *)
    let () = assert (B.is_empty prefix && left = None && right = None) in
    B.first suffix
  else
    (* This 5-tuple is not suffix-only. Its prefix must be nonempty. *)
    B.first prefix

(* [prepare_pop_case_1 f t left] requires the 5-tuple [f] to have
   a prefix buffer of length 3. Furthermore, it expects the left deque
   of [f] to have been already decomposed into one triple [t] followed
   with a deque [left]. It returns a 5-tuple that is equivalent to [f]
   and that is an acceptable argument for [naive_pop]. *)

(* The pair [t, left] may have been produced either by [naive_pop] or
   by [pop_nonempty]. In the former case, the deque [left] is invalid,
   and must be repaired by a [push] operation. In the code below, the
   [push] operations that (potentially) repair the data structure are
   followed with [assert (check left; true)]. This assertion ensures
   that the repair is successful. *)

let[@inline] prepare_pop_case_1 (type a)
  (f : a five_tuple) (t : a triple) (left : a triple deque)
: a five_tuple =
  let { prefix; _ } = f in
  assert (B.length prefix = 3);
  let { first; child; last } = t in
  (* The buffer [first] has length 3 or 2.
     This gives rise to two subcases. *)
  assert (is_ordinary first);

  if B.has_length_3 first then
    (* Move one element from [first], towards the left, into [prefix]. *)
    let prefix, first = B.move_left_1_33 prefix first in
    let t = triple first child last in
    let left = push t left in
    assert (check left; true);
    { f with prefix; left }

  else
    (* Move all elements from [first], towards the left, into [prefix]. *)
    let prefix = B.concat32 prefix first in
    (* If [child] and [last] are both empty, then we are done. *)
    if is_empty child && B.is_empty last then
      (* Here, because [child] is empty and [first] does not have length 3,
         the deque [left] cannot have been produced by [naive_pop]. Thus,
         no repairing [push] is needed. *)
      let () = assert (check left; true) in
      { f with prefix; left }
    (* Otherwise, *)
    else
      (* When [child] is nonempty, [last] is nonempty. *)
      (* Therefore, here, [last] must be nonempty. *)
      let () = assert (not (B.is_empty last)) in
      let t = buffer last in
      let left = push t left in
      assert (check left; true);
      let left = concat child left in
      { f with prefix; left }

let[@inline] prepare_pop_case_2 (type a)
  (f : a five_tuple) (t : a triple) (right : a triple deque)
: a five_tuple =
  let { prefix; middle; _ } = f in
  assert (B.length prefix = 3);
  assert (B.length middle = 2);
  let { first; child; last } = t in
  assert (is_ordinary first);

  if B.has_length_3 first then
    let m0, m1 = B.decompose_doubleton middle in
    (* Move one element from [middle], to the left, into [prefix]. *)
    let prefix = B.inject prefix m0 in
    (* Move one element from [first], to the left, into [middle]. *)
    let m2, first = B.pop first in
    let middle = B.doubleton m1 m2 in
    let t = triple first child last in
    let right = push t right in
    assert (check right; true);
    { f with prefix; middle; right }

  else
    let prefix = B.concat32 prefix middle in
    let right =
      if is_empty child && B.is_empty last then right
      else concat child (push (buffer last) right)
    in
    let middle = first in
    { f with prefix; middle; right }

let rec pop_nonempty : type a. a nonempty_deque -> a * a deque = fun r ->
  let f = !r in
  (* If [naive_pop] is safe now, then just do it. Otherwise, update
     the data structure so that [naive_pop] is safe, and do it. *)
  if naive_pop_safe f then
    naive_pop f
  else
    let f = prepare_pop f in
    r := f;
    assert (naive_pop_safe f);
    naive_pop f

(* TODO when writing symmetric code for [eject], remember that the
   triple [t] must be normalized in the other direction *)
and pop_triple_nonempty : type a. a triple nonempty_deque -> a triple * a triple deque = fun r ->
  let f = !r in
  (* Inspect the first triple in [f]. *)
  let t = inspect_first f in
  (* If this triple satisfies a certain mysterious condition, then extract it
     using [naive_pop]; otherwise extract it using [pop_nonempty]. This use of
     [naive_pop] is not "safe"; it produces a deque whose invariant does not
     necessarily hold. This deque must be repaired by a [push] operation,
     which replaces the triple [t] with a new triple. *)
  (* The reader might wonder why we do not just call [pop_nonempty] always, as
     it is unconditionally safe and functionally equivalent to [naive_pop].
     The answer is, [pop_nonempty] is more expensive than [naive_pop].
     Achieving the desired asymptotic complexity requires care here. *)
  if not (is_empty t.child) || B.has_length_3 t.first then
    naive_pop f
  else
    pop_nonempty r

and prepare_pop : type a. a five_tuple -> a five_tuple = fun f ->
  let { prefix; left; middle; right; suffix } = f in
  assert (B.length prefix = 3);
  match left, right with

  | Some r, _ ->
      (* Case 1: [left] is a nonempty deque. *)
      (* Extract one triple [t] out of [left],
         then jump to the function that handles case 1. *)
      let t, left = pop_triple_nonempty r in
      prepare_pop_case_1 f t left

  | None, Some r ->
      (* Case 2: [left] is empty; [right] is nonempty. *)
      (* [middle] has length 2. *)
      assert (B.length middle = 2);
      let t, right = pop_triple_nonempty r in
      prepare_pop_case_2 f t right

  | None, None ->
      (* Case 3 in the paper: [left] and [right] are empty. *)
    if B.has_length_3 suffix
      then let suffix = B.concat3x prefix (B.fold_left B.inject middle suffix)
            in { f with middle = B.empty; prefix = B.empty; suffix }
    else
      let a, m = B.pop middle in
      let prefix = B.inject prefix a in
      let a, suffix = B.pop suffix in
      let middle = B.inject m a in
      { f with prefix; middle; suffix }

let pop d =
  match d with
  | None ->
      assert false
  | Some r ->
      pop_nonempty r

let pop_opt d =
  match d with
  | None ->
      None
  | Some r ->
      Some (pop_nonempty r)

(* -------------------------------------------------------------------------- *)

let naive_eject : type a. a five_tuple -> a deque * a =
  fun m ->
  let { prefix; left; middle; right; suffix } = m in
    let suffix, x = B.eject suffix in
    assemble prefix left middle right suffix, x

let last_nonempty tr =
  if not (B.is_empty tr.last)
  then Some tr.last
  else if not (B.is_empty tr.first)
  then Some tr.first
  else None

let inspect_last : type a. a five_tuple -> a =
  fun m -> B.last m.suffix

let rec eject_nonempty : type a. a nonempty_deque -> a deque * a =
  fun ptr ->
  let { prefix; left; middle; right; suffix } as d = !ptr in
  if B.is_empty middle || B.length suffix > 3 then
    naive_eject d
  else
  let balanced_deque = begin
  assert(B.length suffix = 3);
  match left, right with
    | _, Some right (* not empty *) ->
      let rightm = !right in
      let t = inspect_last rightm in
      let l, t = match last_nonempty t with
        | Some b when B.has_length_3 b
            -> naive_eject rightm
        | _ when not (is_empty t.child)
            -> naive_eject rightm
        | _ -> eject_nonempty right
      in
      let { first = x; child = d'; last = y } = t in
      begin match B.length x, B.length y with
      | _, 3 ->
        let y', a = B.eject y in
        let s' = B.push a suffix in
        let rd' = inject l (triple x d' y') in
        { d with suffix = s'; right = rd' }
      | _, 2 ->
        let s' = B.fold_right B.push y suffix in
        if is_empty d' && B.is_empty x
          then { d with suffix = s'; right = l }
        else (* NOTE(Juliette): the paper is phrased in a way that contradicts this code but leads to errors *)
          let l' = concat (inject l (buffer x)) d'
          in { d with suffix = s'; right = l' }
      | 3, 0 ->
        (* y is empty *therefore* d' is empty  *)
        assert (is_empty d');
        let x', a = B.eject x in
        let s' = B.push a suffix in
        let rd' = inject l (triple x' d' y) in
        { d with suffix = s'; right = rd' }
      | 2, 0 ->
        let s' = B.fold_right B.push x suffix in
        (* here we know y and d' are empty *)
        { d with suffix = s'; right = l }
      | _ -> assert false
      end
    | Some left, None ->
      let leftm = !left in
      let t = inspect_last leftm in
      let (r, t) = match last_nonempty t with
        | Some b when B.has_length_3 b
            -> naive_eject leftm
        | _ when not (is_empty t.child)
            -> naive_eject leftm
        | _ -> eject_nonempty left
      in
      let { first = x; child = d'; last = y } = t in
      begin match B.length x, B.length y with
      | _, 3 ->
        let m, a = B.eject middle in
        let s = B.push a suffix in
        let y', a = B.eject y in
        let m' = B.push a m in
        let l' = inject r (triple x d' y') in
        { d with prefix = s; middle = m'; left = l' }
      | _, 2 ->
        let s = B.fold_right B.push middle suffix in
        let l' = if is_empty d' && B.is_empty x
            then r else concat d' (push (buffer x) r)
        in
        { d with suffix = s; middle = y; left = l' }
      | 3, 0 ->
        (* x is empty therefore d' is empty too *)
        let m, a = B.eject middle in
        let s = B.push a suffix in
        let x', a = B.eject x in
        let m' = B.push a m in
        let l' = inject r (triple x' d' y) in
        { d with suffix = s; middle = m'; left = l' }
      | 2, 0 ->
        let s = B.fold_right B.push middle suffix in
        { d with suffix = s; middle = x; left = r }
      | _ -> assert false
      end
    | _ (* is_empty left, is_empty right *) ->
      if B.has_length_3 prefix
        then let suffix = B.concat3x prefix (B.fold_left B.inject middle suffix)
              in { d with middle = B.empty; prefix = B.empty; suffix }
      else
        let m, a = B.eject middle in
        let suffix = B.push a suffix in
        let prefix, a = B.eject prefix in
        let middle = B.push a m in
        { d with prefix; middle; suffix }
    end
  in
    ptr := balanced_deque;
    naive_eject balanced_deque

let eject d =
  match d with
  | None ->
      assert false
  | Some r ->
      eject_nonempty r

let eject_opt d =
  match d with
  | None ->
      None
  | Some r ->
      Some (eject_nonempty r)

(* -------------------------------------------------------------------------- *)

(* Map. *)

(* [map] does not preserve sharing: if a reference is reachable via several
   paths in the original deque then [map] creates several distinct copies of
   this reference. In the time complexity analysis, this may at first sight
   appear problematic, as each copy of the reference needs its own copy of
   the reference's potential (time credits). Fortunately, this can be
   achieved by having [map] require [K.n] time credits for a sufficiently
   large constant [K]. This argument relies on the fact that the number of
   references in a deque is at most [n] -- in other words, the number of
   5-tuples in a deque is at most the number of elements. This is true
   because there are no empty 5-tuples. *)

let rec map : type a b. (a -> b) -> a deque -> b deque = fun f d ->
  match d with
  | None ->
      None
  | Some r ->
      let { prefix; left; middle; right; suffix } = !r in
      let prefix = B.map f prefix in
      let left = map (map_triple f) left in
      let middle = B.map f middle in
      let right = map (map_triple f) right in
      let suffix = B.map f suffix in
      Some (ref { prefix; left; middle; right; suffix })

and map_triple : type a b. (a -> b) -> a triple -> b triple = fun f t ->
  let { first; child; last } = t in
  let first = B.map f first in
  let child = map (map_triple f) child in
  let last = B.map f last in
  { first; child; last }

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let rec fold_left : type a s. (s -> a -> s) -> s -> a deque -> s =
  fun f s d ->
  match d with
  | None ->
      s
  | Some r ->
      let { prefix; left; middle; right; suffix } = !r in
      let s = B.fold_left f s prefix in
      let s = fold_left (fold_left_triple f) s left in
      let s = B.fold_left f s middle in
      let s = fold_left (fold_left_triple f) s right in
      let s = B.fold_left f s suffix in
      s

and fold_left_triple : type a s. (s -> a -> s) -> s -> a triple -> s =
  fun f s t ->
  let { first; child; last } = t in
  let s = B.fold_left f s first in
  let s = fold_left (fold_left_triple f) s child in
  let s = B.fold_left f s last in
  s

let rec fold_right : type a s. (a -> s -> s) -> a deque -> s -> s =
  fun f d s ->
  match d with
  | None ->
      s
  | Some r ->
      let { prefix; left; middle; right; suffix } = !r in
      let s = B.fold_right f suffix s in
      let s = fold_right (fold_right_triple f) right s in
      let s = B.fold_right f middle s in
      let s = fold_right (fold_right_triple f) left s in
      let s = B.fold_right f prefix s in
      s

and fold_right_triple : type a s. (a -> s -> s) -> a triple -> s -> s =
  fun f t s ->
  let { first; child; last } = t in
  let s = B.fold_right f last s in
  let s = fold_right (fold_right_triple f) child s in
  let s = B.fold_right f first s in
  s

(* -------------------------------------------------------------------------- *)

(* Length. *)

let[@inline] length d =
  fold_left (fun s _x -> s + 1) 0 d
