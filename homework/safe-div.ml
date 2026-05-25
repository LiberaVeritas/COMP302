(* GRADE:  100% *)

(* Implement safe_div which protects against division by zero. *)
let safe_div (n : int) (d : int) : int option =
  match d with
  | 0 -> None
  | _ -> Some (n / d)

(* Implement safe_div2, which does division twice using safe_div. *)
let safe_div2 (n : int) (d1 : int) (d2 : int) : int option =
  match safe_div n d1 with
  | None -> None
  | Some n2 -> safe_div n2 d2

