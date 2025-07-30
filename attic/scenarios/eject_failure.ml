(* @04: Failure in an instruction. *)
          open Kot.Deque;;
          let rec batch_inject n xs x =
            if n = 0 then xs else
            let xs = inject xs x in
            batch_inject (n-1) xs x;;
          let rec batch_eject xs n =
            if n = 1 then eject xs
            else let xs, _ = eject xs in
            batch_eject xs (n-1);;
(* @01 *) let x0 = empty;;
(* @02 *) let x1 = batch_inject 9 x0 29;;
(* @03 *) let x2 = concat x1 x1;;
(* @04 *) let x3, _ = batch_eject x2 6;;
          let () = check x3;;
