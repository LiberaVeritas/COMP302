(* GRADE:  100% *)
let perimeter (shape : shape) : float = 
  match shape with
  | Circle r -> 2. *. pi *. r
  | Square s -> 4. *. s
  | Rectangle (l, w) -> 2. *. l +. 2. *. w

let area (shape : shape) : float = 
  match shape with
  | Circle r -> pi *. r**2.
  | Square s -> s**2.
  | Rectangle (l, w) -> l *. w

