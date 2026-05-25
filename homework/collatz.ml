(* GRADE:  100% *)

let even n = n mod 2 = 0
             
let hailstone (n : int) : int = 
  let rec hailstone' (x: int) (acc: int) =
    match x with
    | 1 -> acc 
    | e when even e -> hailstone' (e / 2) (acc + 1)
    | x -> hailstone' (3 * x + 1) (acc + 1)
  in
  hailstone' n 0
  

