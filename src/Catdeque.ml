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
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    let left_size = reduce (+) size_triple 0 left_deque in
    let right_size = reduce (+) size_triple 0 right_deque in
    B.size prefix + B.size middle + B.size suffix + left_size + right_size
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
        assert (B.size middle = 2);
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

let is_suffix_only {middle; _} = B.is_empty middle

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
    if B.size suffix = 0 then empty
    else 
      Some (ref { prefix; left_deque; middle; right_deque; suffix })
  end
  else
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
    if B.size suffix = 8 then begin
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
    end
    else
      assemble B.empty empty B.empty empty (B.push x0 suffix)
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

let rec inject : type a. a catdeque -> a -> a catdeque =
  fun c x0 ->
  match c with
  | None -> singleton x0
  | Some r when is_suffix_only (!r) ->
    let suffix = (!r).suffix in
    if B.size suffix = 8 then begin
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
      let suffix = B.inject suffix x0 in
      assemble prefix left_deque middle right_deque suffix
    end
    else
      assemble B.empty empty B.empty empty (B.inject suffix x0)
  | Some r (* when not (is_suffix_only (!r)) *) ->
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    if B.size suffix = 6
    then begin
      let x1, suffix = B.pop suffix in
      let x2, suffix = B.pop suffix in
      let suffix' = B.push x1 (B.push x2 B.empty) in
      let right_deque = inject right_deque (triple B.empty empty suffix') in
      r := { prefix; left_deque; middle; right_deque; suffix }
    end;
    let { prefix; left_deque; middle; right_deque; suffix } = !r in
    let suffix = B.inject suffix x0 in
    assemble prefix left_deque middle right_deque suffix

let flip f a b = f b a

(* partitions a buffer into two buffers containing two or three elements, possibly leaving the second one empty *)
let partition_buffer_left b =
  let s = B.size b in
  if s <= 3
    then b, B.empty
  else if s = 4 then
    let b, x1 = B.eject b in
    let b, x0 = B.eject b in
    b, B.push x0 (B.push x1 B.empty)
  else
    let b, x2 = B.eject b in
    let b, x1 = B.eject b in
    let b, x0 = B.eject b in
    b, B.push x0 (B.push x1 (B.push x2 B.empty))

(* partitions a buffer into two buffers containing two or three elements, possibly leaving the first one empty *)
let partition_buffer_right b =
  let s = B.size b in
  if s <= 3
    then B.empty, b
  else if s = 4 then
    let b, x1 = B.eject b in
    let b, x0 = B.eject b in
    b, B.push x0 (B.push x1 B.empty)
  else
    let b, x2 = B.eject b in
    let b, x1 = B.eject b in
    let b, x0 = B.eject b in
    b, B.push x0 (B.push x1 (B.push x2 B.empty))

let push_buffer push b d2 = 
    (* TODO(Juliette): rewrite more elegantly maybe? *)
    let sf = ref b in
    let d2 = ref d2 in
    while not (B.is_empty !sf) do
      let sf', x = B.eject !sf in
      sf := sf';
      d2 := push x !d2
    done;
    !d2

let concat : type a. a catdeque -> a catdeque -> a catdeque =
  fun d1 d2 ->
  match d1, d2 with
  | None, _ -> d2
  | _, None -> d1
  | Some r1, Some r2 
  (* both are 5-tuples *)
  when not (is_suffix_only (!r1) || is_suffix_only (!r2)) ->
    let { prefix = pr1; left_deque = ld1; middle = md1; right_deque = rd1; suffix = sf1 } = !r1 in
    let { prefix = pr2; left_deque = ld2; middle = md2; right_deque = rd2; suffix = sf2 } = !r2 in
    let y, pr2' = B.pop pr2 in
    let sf1', x = B.eject sf1 in
    let middle = B.push x (B.push y B.empty) in
    let s1', s1'' = partition_buffer_left sf1' in
    let ld1' = inject ld1 (triple md1 rd1 s1') in
    let ld1'' = if B.is_empty s1'' then ld1'
                else inject ld1' (triple s1'' empty B.empty) in
    let p2', p2'' = partition_buffer_right pr2' in
    let rd2' = push (triple p2'' ld2 md2) rd2 in
    let rd2'' = if B.is_empty p2' then rd2'
                else push (triple p2' empty B.empty) rd2' in
    assemble pr1 ld1'' middle rd2'' sf2
  | _, Some r2 when is_suffix_only (!r2) ->
    B.fold_left inject d1 (!r2).suffix
  | Some r1, _ (* is_suffix_only (!r2) *) ->
    push_buffer push (!r1).suffix d2

let naive_pop : type a. a nonempty_catdeque -> a * a catdeque =
  fun r ->
  let { prefix; left_deque; middle; right_deque; suffix } as m = !r in
  if is_suffix_only m
  then
    let x, suffix = B.pop suffix in
    x, assemble prefix left_deque middle right_deque suffix
  else
    let x, prefix = B.pop prefix in
    x, assemble prefix left_deque middle right_deque suffix

let first_nonempty tr =
  if not (B.is_empty tr.first)
  then Some (tr.first)
  else if not (B.is_empty tr.last)
  then Some (tr.last)
  else None

let inspect_first : type a. a nonempty_catdeque -> a =
  fun r ->
  let { prefix; suffix; _ } as m = !r in
  if is_suffix_only m then fst (B.pop suffix) else
  (* not suffix-only, prefix is nonempty *)
  fst (B.pop prefix) 
  
let rec pop_nonempty : type a. a nonempty_catdeque -> a * a catdeque =
  fun ptr ->
  let { prefix; left_deque; middle; right_deque; suffix } as d = !ptr in
  if not (is_suffix_only d || B.size prefix > 3) then begin
  (* B.size prefix = 3 *)
  match left_deque, right_deque with
    | Some left_deque (* not empty *), _ ->
      let t = inspect_first left_deque in
      let (t, l) = match first_nonempty t with
        | Some b when B.size b = 3
            -> naive_pop left_deque
        | None when not (is_empty t.child)
            -> naive_pop left_deque
        | _ -> pop_nonempty left_deque
      in 
      let { first = x; child = d'; last = y } = t in
      begin match B.size x, B.size y with
      | 3, _ ->
        let a, x' = B.pop x in
        let p' = B.inject prefix a in
        let ld' = push (triple x' d' y) l in
        ptr := { d with prefix = p'; left_deque = ld' }
      | 2, _ ->
        let p' = B.(fold_left inject prefix x) in
        if is_empty d' && B.is_empty y
          then ptr := { d with prefix = p'; left_deque = l }
        else (* NOTE(Juliette): the paper is phrased in a way that contradicts this code but leads to errors *)
          let l' = concat d' (push (triple y empty B.empty) l) 
          in ptr := { d with prefix = p'; left_deque = l' }
      | 0, 3 -> 
        (* x is empty *therefore* d' is empty  *)
        assert (is_empty d');
        let a, y' = B.pop y in
        let p' = B.inject prefix a in
        let ld' = push (triple x d' y') l in
        ptr := { d with prefix = p'; left_deque = ld' }
      | 0, 2 ->
        let p' = B.fold_left B.inject prefix y in
        (* here we know x and d' are empty *)
        ptr := { d with prefix = p'; left_deque = l }
      | _ -> assert false
      end
    | None, Some right_deque ->
      let t = inspect_first right_deque in
      let (t, r) = match first_nonempty t with
        | Some b when B.size b = 3
            -> naive_pop right_deque
        | _ when not (is_empty t.child)
            -> naive_pop right_deque
        | _ -> pop_nonempty right_deque
      in
      let { first = x; child = d'; last = y } = t in
      begin match B.size x, B.size y with
      | 3, _ ->
        let a, m = B.pop middle in
        let p = B.inject prefix a in
        let b, x' = B.pop x in
        let m' = B.inject m b in
        let r' = push (triple x' d' y) r in
        ptr := { d with prefix = p; middle = m'; right_deque = r' }
      | 2, _ ->
        let p = B.(fold_left inject prefix middle) in
        let r' = if is_empty d' && B.is_empty y
            then r else concat d' (push (triple y empty B.empty) r)
        in
        ptr := { d with prefix = p; middle = x; right_deque = r' }
      | 0, 3 ->
        (* x is empty therefore d' is empty too *)
        let a, m = B.pop middle in
        let p = B.inject prefix a in
        let b, y' = B.pop y in
        let m' = B.inject m b in
        let r' = push (triple x d' y') r in
        ptr := { d with prefix = p; middle = m'; right_deque = r' }
      | 0, 2 ->
        let p = B.(fold_left inject prefix middle) in
        ptr := { d with prefix = p; middle = y; right_deque = r }
      | _ -> assert false
      end
    | _ (* is_empty left_deque, is_empty right_deque *) ->
      if B.size suffix = 3
        then let suffix = B.(fold_left inject prefix (fold_left inject middle suffix))
              in ptr := { d with middle = B.empty; prefix = B.empty; suffix }
      else
        let a, m = B.pop middle in
        let prefix = B.inject prefix a in
        let a, suffix = B.pop suffix in
        let middle = B.inject m a in
        ptr := { d with prefix; middle; suffix }
    end;
  naive_pop ptr

let pop : type a. a catdeque -> a * a catdeque
  = function
  | None -> assert false
  | Some r -> pop_nonempty r

let pop_opt : type a. a catdeque -> (a * a catdeque) option 
  = fun x -> Option.map pop_nonempty x

(* travaux *)

let naive_eject : type a. a nonempty_catdeque -> a catdeque * a =
  fun r ->
  let { prefix; left_deque; middle; right_deque; suffix } = !r in
    let suffix, x = B.eject suffix in
    assemble prefix left_deque middle right_deque suffix, x

let last_nonempty tr =
  if not (B.is_empty tr.last)
  then Some tr.last
  else if not (B.is_empty tr.first)
  then Some tr.first
  else None

let inspect_last : type a. a nonempty_catdeque -> a =
  fun r -> snd (B.eject (!r).suffix) 
  
let rec eject_nonempty : type a. a nonempty_catdeque -> a catdeque * a =
  fun ptr ->
  let { prefix; left_deque; middle; right_deque; suffix } as d = !ptr in
  if not (is_suffix_only d || B.size suffix > 3) then begin
  (* B.size prefix = 3 *)
  match left_deque, right_deque with
    | _, Some right_deque (* not empty *) ->
      let t = inspect_last right_deque in
      let l, t = match last_nonempty t with
        | Some b when B.size b = 3
            -> naive_eject right_deque
        | None when not (is_empty t.child)
            -> naive_eject right_deque
        | _ -> eject_nonempty right_deque
      in 
      let { first = x; child = d'; last = y } = t in
      begin match B.size x, B.size y with
      | _, 3 ->
        let y', a = B.eject y in
        let s' = B.push a suffix in
        let rd' = inject l (triple x d' y') in
        ptr := { d with suffix = s'; right_deque = rd' }
      | _, 2 ->
        let s' = push_buffer B.push y suffix in
        if is_empty d' && B.is_empty x
          then ptr := { d with suffix = s'; right_deque = l }
        else (* NOTE(Juliette): the paper is phrased in a way that contradicts this code but leads to errors *)
          let l' = concat (inject l (triple B.empty empty x)) d'
          in ptr := { d with suffix = s'; right_deque = l' }
      | 3, 0 -> 
        (* y is empty *therefore* d' is empty  *)
        assert (is_empty d');
        let x', a = B.eject x in
        let s' = B.push a suffix in
        let rd' = inject l (triple x' d' y) in
        ptr := { d with suffix = s'; right_deque = rd' }
      | 2, 0 ->
        let s' = push_buffer B.push x suffix in
        (* here we know y and d' are empty *)
        ptr := { d with suffix = s'; right_deque = l }
      | _ -> assert false
      end
    | Some left_deque, None ->
      let t = inspect_last left_deque in
      let (r, t) = match last_nonempty t with
        | Some b when B.size b = 3
            -> naive_eject left_deque
        | _ when not (is_empty t.child)
            -> naive_eject left_deque
        | _ -> eject_nonempty left_deque
      in
      let { first = x; child = d'; last = y } = t in
      begin match B.size x, B.size y with
      | _, 3 ->
        let m, a = B.eject middle in
        let s = B.push a suffix in
        let y', a = B.eject y in
        let m' = B.push a m in
        let l' = inject r (triple x d' y') in
        ptr := { d with prefix = s; middle = m'; left_deque = l' }
      | _, 2 ->
        let s = push_buffer B.push middle suffix in
        let l' = if is_empty d' && B.is_empty x
            then r else concat d' (push (triple x empty B.empty) r)
        in
        ptr := { d with suffix = s; middle = y; left_deque = l' }
      | 3, 0 ->
        (* x is empty therefore d' is empty too *)
        let m, a = B.eject middle in
        let s = B.push a suffix in
        let x', a = B.eject x in
        let m' = B.push a m in
        let l' = inject r (triple x' d' y) in
        ptr := { d with suffix = s; middle = m'; left_deque = l' }
      | 2, 0 ->
        let s = push_buffer B.push middle suffix in
        ptr := { d with suffix = s; middle = x; left_deque = r }
      | _ -> assert false
      end
    | _ (* is_empty left_deque, is_empty right_deque *) ->
      if B.size prefix = 3
        then let suffix = B.(fold_left inject prefix (fold_left inject middle suffix))
              in ptr := { d with middle = B.empty; prefix = B.empty; suffix }
      else
        let m, a = B.eject middle in
        let suffix = B.push a suffix in
        let prefix, a = B.eject prefix in
        let middle = B.push a m in
        ptr := { d with prefix; middle; suffix }
    end;
  naive_eject ptr

let eject : type a. a catdeque -> a catdeque * a
  = function
  | None -> assert false
  | Some r -> eject_nonempty r

let eject_opt : type a. a catdeque -> (a catdeque * a) option 
  = fun x -> Option.map eject_nonempty x


(* travaux terminés *)

let show_list (show_elem : 'a -> string) (lst : 'a list) : string =
  let contents = lst |> List.map show_elem |> String.concat "; " in
  Printf.sprintf "[%s]" contents

let show_buffer : type a. (a -> string) -> a buffer -> string =
  fun show_elem b ->
  show_list show_elem (B.fold_left (fun b a -> b @ [a]) [] b)

let indent_more (s : string) : string = s ^ "  "

let rec show_triple : type a. (a -> string) -> string -> a triple -> string =
  fun show_elem indent t ->
  Printf.sprintf
    "{\n%s  first = %s;\n%s  child = %s;\n%s  last = %s\n%s}"
    indent (show_buffer show_elem t.first)
    indent (show_catdeque (show_triple show_elem indent) (indent_more indent) t.child)
    indent (show_buffer show_elem t.last)
    indent

and show_five_tuple : type a. (a -> string) -> string -> (a five_tuple ref) -> string =
  fun show_elem indent r ->
  let { prefix; left_deque; middle; right_deque; suffix } = !r in
  Printf.sprintf
    "{\n%s  prefix = %s;\n%s  left_deque = %s;\n%s  middle = %s;\n%s  right_deque = %s;\n%s  suffix = %s\n%s}"
    indent (show_buffer show_elem prefix)
    indent (show_catdeque (show_triple show_elem (indent_more indent)) (indent_more indent) left_deque)
    indent (show_buffer show_elem middle)
    indent (show_catdeque (show_triple show_elem (indent_more indent)) (indent_more indent) right_deque)
    indent (show_buffer show_elem suffix)
    indent

and show_catdeque : type a. (a -> string) -> string -> a catdeque -> string =
  fun show_elem indent cd ->
  match cd with
  | None -> "empty"
  | Some nonempty ->
    Printf.sprintf "Some (ref %s)" (show_five_tuple show_elem (indent_more indent) nonempty)

let debug cd = print_endline (show_catdeque string_of_int "" cd)
