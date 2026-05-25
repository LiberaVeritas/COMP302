(* GRADE:  100% *)

let rec sum (start : int) (stop : int) : int =
  if (start = stop) then stop
  else start + sum (start + 1) stop

(* To make testing easier, define your helper function for sum_tr
   here at the top level. You should change the names of the arguments
   to something more descriptive. You can also add more if you want. *)
let rec sum_tr_go stop n acc = 
  if (n = stop) then acc + stop
  else sum_tr_go stop (n + 1) (acc + n)
      
let sum_tr start stop = 
  if (start = stop) then stop
  else sum_tr_go stop start 0

let sum_nr start stop = (start + stop) * (stop - start + 1) / 2

