(* GRADE:  100% *)
let must_stop (distance : int) (light : traffic_light) : bool = 
  match light with
  | Red -> true
  | Green -> false
  | Yellow -> distance > 100

