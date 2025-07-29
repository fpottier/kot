(* This is a scenario where [naive_pop f] is invoked and
   [naive_pop_safe f] does not hold. *)

          open Kot.Deque;;
          let rec batch_push n x xs =
            if n = 0 then xs else
            let xs = push x xs in
            batch_push (n-1) x xs
          let rec batch_inject n xs x =
            if n = 0 then xs else
            let xs = inject xs x in
            batch_inject (n-1) xs x
(* @01 *) let x0 = empty;;
(* @02 *) let x1 = batch_inject 24 x0 14;;
(* @03 *) let x2 = concat x1 x1;;
(* @04 *) let x3 = concat x2 x1;;
(* @05 *) let x4 = concat x3 x1;;
(* @07 *) let x6 = concat x4 x1;;
(* @08 *) let x7 = concat x6 x4;;
(* @09 *) let _ = pop x7;;
