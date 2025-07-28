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
  let fo = B.length_is_between first 2 3 in
  let lo = B.length_is_between  last 2 3 in
  let fe = B.is_empty first in
  let le = B.is_empty  last in
  match child with
  | None ->
      (* When [child] is empty, either both [first] and [last] are ordinary
         (which means that they have size 2 or 3) or one of them is empty
         and the other is ordinary. *)
      assert (fo && lo || fe && lo || fo && le)
  | Some _ ->
      (* When [child] is nonempty, both [first] and [last] are ordinary. *)
      assert (fo && lo)

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
  { first; child; last }

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
          let left = push (triple prefix' empty B.empty) left in
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
          let right = inject right (triple B.empty empty suffix') in
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

(* [naive_pop_permitted f] determines whether the call [naive_pop f]
   is legal.
   TODO unclear whether this is *the* precondition of [naive_pop]
   or *one of the possible preconditions* of [naive_pop] *)

let[@inline] naive_pop_permitted (f : 'a five_tuple) : bool =
  let { prefix; middle; _ } = f in
  B.is_empty middle || B.length prefix > 3

(* [naive_pop] expects a 5-tuple, which must satisfy the precondition
   [B.is_empty middle || B.length prefix > 3]. In this special case,
   it performs a [pop] operation. *)

let naive_pop (type a) (f : a five_tuple) : a * a deque =
  (* TODO unclear whether this assertion is correct!
     in the tests it is never violated,
     but some call sites of [naive_pop] do not clearly obey it *)
  assert (naive_pop_permitted f);
  let { prefix; left; middle; right; suffix } = f in
  if B.is_empty middle then
    let x, suffix = B.pop suffix in
    x, assemble prefix left middle right suffix
  else
    let x, prefix = B.pop prefix in
    x, assemble_ prefix left middle right suffix

let first_nonempty_buffer (type a) (t : a triple) : a buffer =
  let { first; child; last } = t in
  if not (B.is_empty first) then
    (* The buffer [first] is nonempty. *)
    first
  else begin
    (* The buffer [first] is empty. The child deque must be empty as well,
       and the buffer [last] must be nonempty. *)
    assert (is_empty child);
    assert (not (B.is_empty last));
    last
  end

let inspect_first (type a) (f : a five_tuple) : a =
  let { prefix; middle; suffix; _ } = f in
  if B.is_empty middle then begin
    (* This 5-tuple is suffix-only. *)
    assert (B.is_empty prefix);
    B.first suffix
  end
  else
    (* This 5-tuple is not suffix-only. Its prefix must be nonempty. *)
    B.first prefix

let[@inline] prepare_naive_pop_case_1 (type a)
  (f : a five_tuple) (t : a triple) (left : a triple deque)
: a five_tuple =
  let { prefix; _ } = f in
  let { first; child; last } = t in
  (* The buffer [first] has length 3, 2, or 0.
     This gives rise to three subcases. *)
  let lfirst = B.length first in
  assert (lfirst = 3 || lfirst = 2 || lfirst = 0);
  match lfirst with

  | 3 ->
      (* Move one element from [first], towards the left, into [prefix]. *)
      let x, first = B.pop first in
      let prefix = B.inject prefix x in
      let t = triple first child last in
      let left = push t left in
      { f with prefix; left }

  | 2 ->
      (* Move all elements from [first], towards the left, into [prefix]. *)
      let prefix = B.concat3x prefix first in
      (* If [child] and [last] are both empty, then we are done. *)
      if is_empty child && B.is_empty last then
        { f with prefix; left }
      (* Here, the paper is phrased in a way that is slightly unclear.
         The fragment "If, on the other hand, d' and y are not null"
         should be replaced with just "Otherwise". *)
      (* Otherwise, *)
      else
        (* When [child] is nonempty, [last] is nonempty. *)
        (* Therefore, here, [last] must be nonempty. *)
        let () = assert (not (B.is_empty last)) in
        let t = triple last empty B.empty in
        let left = push t left in
        let left = concat child left in
        { f with prefix; left }

  | _ (* 0 *) ->
      assert (lfirst = 0);
      (* Because [first] is empty, [child] is empty as well.
         Therefore [last] must be nonempty. *)
      assert (is_empty child);
      assert (not (B.is_empty last));
      (* The buffer [last] has length 3 or 2.
         This gives rise to two subsubcases. *)
      let llast = B.length last in
      assert (llast = 3 || llast = 2);
      if llast = 3 then
        let a, last = B.pop last in
        let prefix = B.inject prefix a in
        let left = push (triple first child last) left in
        { f with prefix; left }
      else
        let prefix = B.concat3x prefix last in
        { f with prefix; left }

let rec pop_nonempty : type a. a nonempty_deque -> a * a deque = fun r ->
  let f = !r in
  if naive_pop_permitted f then
    naive_pop f
  else
    let f = prepare_naive_pop f in
    r := f;
    naive_pop f

and pop_triple_nonempty : type a. a triple nonempty_deque -> a triple * a triple deque = fun r ->
  let f = !r in
  let t = inspect_first f in
  if not (is_empty t.child) || B.length (first_nonempty_buffer t) = 3 then
    (* TODO unclear why the previous test allows us to call [naive_pop] *)
    naive_pop f
  else
    pop_nonempty r

and prepare_naive_pop : type a. a five_tuple -> a five_tuple = fun f ->
  let { prefix; left; middle; right; suffix } = f in
  assert (B.length prefix = 3);
  match left, right with

  | Some r, _ ->
      (* Case 1: [left] is a nonempty deque. *)
      (* Pop one triple [t] out of [left],
         then jump to the function that handles case 1. *)
      let t, left = pop_triple_nonempty r in
      prepare_naive_pop_case_1 f t left

  | None, Some right ->
      (* Case 2 in the paper: [right] is nonempty. *)
      let t, r = pop_triple_nonempty right in
    let { first = x; child = d'; last = y } = t in
    begin match B.length x, B.length y with
    | 3, _ ->
      let a, m = B.pop middle in
      let p = B.inject prefix a in
      let b, x' = B.pop x in
      let m' = B.inject m b in
      let r' = push (triple x' d' y) r in
      { f with prefix = p; middle = m'; right = r' }
    | 2, _ ->
      let p = B.concat3x prefix middle in
      let r' = if is_empty d' && B.is_empty y
          then r else concat d' (push (triple y empty B.empty) r)
      in
      { f with prefix = p; middle = x; right = r' }
    | 0, 3 ->
      (* x is empty therefore d' is empty too *)
      let a, m = B.pop middle in
      let p = B.inject prefix a in
      let b, y' = B.pop y in
      let m' = B.inject m b in
      let r' = push (triple x d' y') r in
      { f with prefix = p; middle = m'; right = r' }
    | 0, 2 ->
      let p = B.concat3x prefix middle in
      { f with prefix = p; middle = y; right = r }
    | _ -> assert false
    end
  | None, None ->
      (* Case 3 in the paper: [left] and [right] are empty. *)
    if B.length suffix = 3
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
        | Some b when B.length b = 3
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
          let l' = concat (inject l (triple B.empty empty x)) d'
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
        | Some b when B.length b = 3
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
            then r else concat d' (push (triple x empty B.empty) r)
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
      if B.length prefix = 3
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
