(* @12: Failure in an instruction. *)
          open Kot.Deque;;
          let rec batch_inject n xs x =
            if n = 0 then xs else
            let xs = inject xs x in
            batch_inject (n-1) xs x;;
(* @01 *) let x0 = empty;;
(* @02 *) let x1 = batch_inject 19 x0 14;;
(* @03 *) let x2 = concat x1 x1;;
(* @04 *) let x3 = concat x2 x1;;
(* @05 *) let x4 = concat x3 x2;;
(* @06 *) let x5 = concat x4 x2;;
(* @07 *) let x6 = concat x5 x4;;
(* @12 *) let _ = pop x5;;
(* @12: The exception File "src/Deque.ml", line 83, characters 4-10: Assertion failed was raised. *)
