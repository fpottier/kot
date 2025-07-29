(* dune utop *)
(* #use "play.ml" *)
open Kot.Deque;;
(* @01 *) let x0 = empty;;
(* @02 *) let x1 = batch_inject 13 x0 18;;
(* @03 *) let x2 = batch_inject 7 x1 30;;
(* @04 *) let x3 = concat x2 x2;;
(* @05 *) let x4 = concat x3 x2;;
(* @06 *) let x5 = concat x4 x2;;
(* @07 *) let (x6, _) = eject x4;;
(* @08 *) let x7 = concat x5 x5;;
(* @09 *) let x8 = concat x3 x5;;
(* @10 *) let x9 = concat x7 x6;;
(* @11 *) let (_, x10) = pop x3;;
(* @12 *) let _ = pop x7;;
(* @12: The exception File "src/Deque.ml", line 298, characters 2-8: Assertion failed was raised. *)

(* This is a scenario where [naive_pop] is used outside of the
   precondition [naive_pop_permitted]. *)
