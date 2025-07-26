(* dune utop *)
(* #use "play.ml" *)
open Kot.Deque;;
(* @01 *) let x0 = empty;;
(* @02 *) let x1 = inject x0 2;;
(* @03 *) let x2 = push 5 x1;;
(* @04 *) let x3 = concat x2 x2;;
(* @05 *) let x4 = concat x2 x3;;
(* @06 *) let x5 = concat x4 x4;;
          check x5;; (* this check fails! *)
