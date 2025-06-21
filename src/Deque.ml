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

module B = Buffer

type 'a deque =
  'a nonempty_deque option

and 'a nonempty_deque =
  'a triple ref

and 'a triple = {
  prefix: 'a B.buffer;
  child : ('a * 'a) deque;
  suffix: 'a B.buffer;
}

let empty =
  None

let[@inline] is_empty d =
  match d with None -> true | Some _ -> false

let rec check : type a. a deque -> unit = fun d ->
  match d with None -> () | Some d ->
  let { prefix; child; suffix } = !d in
  (* The buffers and child cannot simultaneously be empty. *)
  assert (not (B.is_empty prefix && is_empty child && B.is_empty suffix));
  check child

let rec size : type a. int -> int -> a deque -> int = fun s m d ->
  match d with None -> s | Some d ->
  let { prefix; child; suffix } = !d in
  let s = s + m * (B.size prefix + B.size suffix) in
  let m = 2 * m in
  size s m child

let[@inline] size d =
  size 0 1 d

let[@inline] assemble prefix child suffix =
  if B.is_empty prefix && is_empty child && B.is_empty suffix then
    None
  else
    Some (ref { prefix; child; suffix })

let rec pop_nonempty : type a. a nonempty_deque -> a * a deque = fun d ->
  let { prefix; child; suffix } = !d in
  match B.is_empty prefix, child with
  | true, Some c ->
      (* The front buffer is empty; the child deque is nonempty. *)
      (* Pop a pair [(x, y)] off the child deque. *)
      let (x, y), child = pop_nonempty c in
      (* Inject [x] and [y] into the (empty) front buffer. *)
      (* Update the deque [d] in place. *)
      let prefix = B.B2 (x, y) in
      d := { prefix; child; suffix };
      (* Extract [x]. Create a new deque whose front buffer is a singleton. *)
      let prefix = B.B1 (y) in
      x, assemble prefix child suffix
  | false, _ ->
      (* The front buffer is nonempty. *)
      (* Pop an element [x] off the front buffer. *)
      let x, prefix = B.pop prefix in
      x, assemble prefix child suffix
  | true, None ->
      (* The front buffer and child deque are empty. *)
      (* The rear buffer must be nonempty. *)
      assert (not (B.is_empty suffix));
      (* Pop an element [x] off the rear buffer. *)
      let x, suffix = B.pop suffix in
      x, assemble prefix child suffix

let pop_opt d =
  match d with
  | None ->
      None
  | Some d ->
      Some (pop_nonempty d)

let pop d =
  match d with
  | None ->
      invalid_arg "Deque.pop: deque is empty"
  | Some d ->
      pop_nonempty d

let[@inline] singleton x =
  let prefix = B.B1 (x)
  and child  = None
  and suffix = B.empty in
  Some (ref { prefix; child; suffix })

let rec push_nonempty : type a. a -> a nonempty_deque -> a deque = fun w d ->
  let { prefix; child; suffix } = !d in
  match prefix with
  | B.B3 (x, y, z) ->
      (* The front buffer is full. *)
      (* Extract two elements [y] and [z] out of the front buffer
         and push the pair [(y, z)] into the child deque. *)
      let child = push (y, z) child in
      let prefix = B.B1 (x) in
      (* Update the deque [d] in place. *)
      d := { prefix; child; suffix };
      (* Push [w] into what remains of the front buffer. *)
      let prefix = B.B2 (w, x) in
      Some (ref { prefix; child; suffix })
  | B.B2 _
  | B.B1 _
  | B.B0 ->
      (* Push [w] into the front buffer. *)
      let prefix = B.push w prefix in
      Some (ref { prefix; child; suffix })

and push : type a. a -> a deque -> a deque = fun x d ->
  match d with
  | None ->
      singleton x
  | Some d ->
      push_nonempty x d
