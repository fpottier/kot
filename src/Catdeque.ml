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

module B = Deque
type 'a buffer = 'a Deque.deque

type 'a catdeque =
  'a nonempty_catdeque option
and 'a nonempty_catdeque = 'a five_tuple ref
and 'a five_tuple = {
  (* contains three to six elements (temporarily two during operations) *)
  prefix : 'a buffer;
  left_deque : 'a triple catdeque;
  (* contains exactly two elements *)
  middle : 'a buffer;
  right_deque : 'a triple catdeque;
  (* contains three to six elements (temporarily two during operations) *)
  (* OR one to eight elements in a suffix-only representation *)
  suffix : 'a buffer;
}
(* cannot be empty, and if [child] is nonempty then [first] and [last] are nonempty *)
and 'a triple = {
  (* contains two or three elements *)
  first : 'a buffer;
  child : 'a triple catdeque;
  (* contains two or three elements *)
  last : 'a buffer;
}

let empty = None

let is_empty = Option.is_none

let rec map : type a b. (a -> b) -> a catdeque -> b catdeque = 
  fun f -> 
  function
  | None -> None
  | Some r ->
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    let prefix = B.map f prefix in
    let left_deque = map (map_triple f) left_deque in
    let middle = B.map f middle in
    let right_deque = map (map_triple f) right_deque in
    let suffix = B.map f suffix in
    Some (ref { prefix; left_deque; middle; right_deque; suffix })
and map_triple : type a b. (a -> b) -> a triple -> b triple =
  fun f { first; child; last } ->
  let first = B.map f first in
  let child = map (map_triple f) child in
  let last = B.map f last in
  { first; child; last }


let rec fold_left : type a b. (b -> a -> b) -> b -> a catdeque -> b = 
  fun f y -> 
  function
  | None -> y
  | Some r ->
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    let y = B.fold_left f y prefix in
    let y = fold_left (fold_left_triple f) y left_deque in
    let y = B.fold_left f y middle in
    let y = fold_left (fold_left_triple f) y right_deque in
    let y = B.fold_left f y suffix in
    y
and fold_left_triple : type a b. (b -> a -> b) -> b -> a triple -> b =
  fun f y { first; child; last } ->
  let y = B.fold_left f y first in
  let y = fold_left (fold_left_triple f) y child in
  let y = B.fold_left f y last in
  y

let reduce f g a b = fold_left (fun x y -> f x (g y)) a b

let rec size = function
  | None -> 0
  | Some r ->
    let { prefix; left_deque; right_deque; suffix; _ } = !r in
    let left_size = reduce (+) size_triple 0 left_deque in
    let right_size = reduce (+) size_triple 0 right_deque in
    2 + B.size prefix + B.size suffix + left_size + right_size
and size_triple = function
  | { first; child; last } ->
    let size_child = fold_left (+) 0 (map size_triple child) in
    B.size first + size_child + B.size last

let bounded_size b min max =
  let size = B.size b in
  min <= size && size <= max

let rec check : type a. a catdeque -> unit = function
  | None -> ()
  | Some r ->
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    if B.size middle = 0
      then begin
        assert (B.size prefix = 0);
        assert (size left_deque = 0);
        assert (size right_deque = 0);
        assert (bounded_size suffix 1 8)
      end
      else begin
        assert (B.size middle = 0);
        assert (bounded_size prefix 3 6);
        assert (bounded_size suffix 3 6);
        check left_deque;
        check right_deque;
      end
and check_triple : type a. a triple -> unit = function
  | { first; child; last } ->
    assert (bounded_size first 2 3);
    assert (bounded_size last 2 3);
    check child

let is_suffix_only {middle; _} = B.size middle = 0

let singleton x = Some (ref {
  prefix = B.empty;
  left_deque = empty;
  middle = B.empty;
  right_deque = empty;
  suffix = B.push x B.empty
})

let assemble prefix left_deque middle right_deque suffix =
  if B.size middle = 0
    then begin
      assert (B.size prefix = 0);
      assert (size left_deque = 0);
      assert (size right_deque = 0);
    end;
  Some (ref { prefix; left_deque; middle; right_deque; suffix })

let triple first child last =
  assert (B.size first + B.size last <> 0);
  if not (is_empty child)
    then assert (B.size first * B.size last <> 0);
  { first; child; last }

let rec push : type a. a -> a catdeque -> a catdeque =
  fun x0 c ->
  match c with
  | None -> singleton x0
  | Some r when is_suffix_only (!r) ->
    let suffix = (!r).suffix in
    let x1, suffix = B.pop suffix in
    let x2, suffix = B.pop suffix in
    let x3, suffix = B.pop suffix in
    let prefix = B.push x1 (B.push x2 (B.push x3 B.empty)) in
    let x4, suffix = B.pop suffix in
    let x5, suffix = B.pop suffix in
    let middle = B.push x4 (B.push x5 B.empty) in
    let left_deque = empty in
    let right_deque = empty in
    r := { prefix; left_deque; middle; right_deque; suffix };
    let prefix = B.push x0 prefix in
    assemble prefix left_deque middle right_deque suffix
  | Some r (* when not (is_suffix_only (!r)) *) ->
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    if B.size prefix = 6
    then begin
      let prefix, x6 = B.eject prefix in
      let prefix, x5 = B.eject prefix in
      let prefix' = B.push x5 (B.push x6 B.empty) in
      let left_deque = push (triple prefix' empty B.empty) left_deque in
      r := { prefix; left_deque; middle; right_deque; suffix }
    end;
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    let prefix = B.push x0 prefix in
    assemble prefix left_deque middle right_deque suffix


