(* @14: Failure in a well-formedness check. *)
          open Kot.Deque;;
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
(* @07 *) let (x6, _) = eject x3;;
(* @08 *) let (x7, _) = eject x6;;
(* @11 *) let (x10, _) = eject x7;;
(* @13 *) let (x12, _) = eject x10;;
(* @14 *) let (x13, _) = eject x12;;
          check x13;; (* this check fails! *)
(* @14: The exception File "src/Deque.ml", line 94, characters 8-14: Assertion failed was raised. *)
