(* @14: Failure in a well-formedness check. *)
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
(* @02 *) let x1 = batch_inject 9 x0 11;;
(* @03 *) let (x2, _) = eject x1;;
(* @04 *) let x3 = concat x1 x2;;
(* @05 *) let x4 = batch_inject 26 x1 17;;
(* @06 *) let (x5, _) = eject x3;;
(* @07 *) let (x6, _) = eject x3;;
(* @08 *) let (x7, _) = eject x6;;
(* @09 *) let (x8, _) = eject x5;;
(* @10 *) let x9 = empty;;
(* @11 *) let (x10, _) = eject x7;;
(* @12 *) let x11 = empty;;
(* @13 *) let (x12, _) = eject x10;;
(* @14 *) let (x13, _) = eject x12;;
          check x13;; (* this check fails! *)
(* @14: The exception File "src/Deque.ml", line 94, characters 8-14: Assertion failed was raised. *)
