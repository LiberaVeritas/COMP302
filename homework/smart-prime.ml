(* GRADE:  100% *)
(* You'll want to base this on the implementation from class. *) 

let is_prime (n : int) : bool =
  let m = sqrt n 
  in
  if (n = 2) then true
  else 
    let rec is_prime' i n = 
      if i > m then true
      else if n mod i == 0 then false
      else is_prime' (i+1) n
    in
    is_prime' 2 n
(* Call the function as is_prime 2 n,
   so that i always starts at 2. *)

