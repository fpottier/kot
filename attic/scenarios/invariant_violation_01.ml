(* @09: Failure in an instruction. *)
          open Kot.Deque;;
          let rec batch_push n x xs =
            if n = 0 then xs else
            let xs = push x xs in
            batch_push (n-1) x xs;;
          let rec batch_inject n xs x =
            if n = 0 then xs else
            let xs = inject xs x in
            batch_inject (n-1) xs x;;
          #require "monolith";;
          module Sup = Monolith.Support;;
(* @01 *) let x0 = empty;;
(* @02 *) let x1 = batch_push 17 22 x0;;
(* @03 *) let x2 = concat x1 x1;;
(* @04 *) let x3 = concat x1 x2;;
(* @05 *) let x4 = concat x2 x3;;
(* @06 *) let x5 = concat x1 x4;;
(* @07 *) let x6 = concat x2 x5;;
(* @09 *) let _ = eject x6;;
(* @09: The exception File "src/Deque.ml", line 85, characters 4-10: Assertion failed was raised. *)
