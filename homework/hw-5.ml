(* GRADE:  100% *)


let xt = Variable_map.empty |> Variable_map.add "x" true 
let xf = Variable_map.empty |> Variable_map.add "x" false 
let yt = Variable_map.empty |> Variable_map.add "y" true 
let yf = Variable_map.empty |> Variable_map.add "y" false 
let zt = Variable_map.empty |> Variable_map.add "z" true 
let zf = Variable_map.empty |> Variable_map.add "z" false 
           
let f' k a _ = Some a
  
  
let xtyt = Variable_map.union f' xt yt
let xtyf = Variable_map.union f' xt yf
let xtytzt = Variable_map.union f' xt yt |> Variable_map.union f' zt
let xtytzf = Variable_map.union f' xt yt |> Variable_map.union f' zf
let xtyfzt = Variable_map.union f' xt yf |> Variable_map.union f' zt
let xtyfzf = Variable_map.union f' xt yf |> Variable_map.union f' zf
let xfyfzf = Variable_map.union f' xf yf |> Variable_map.union f' zf
               
let x = Variable "x"
let y = Variable "y"
let z = Variable "z"
    
let ( ! ) x = Negation x
    
let ( * ) a b = Conjunction (a, b)
let ( + ) a b = Disjunction (a, b)
  
    

(* TODO: Add test cases for both Question 1 and 2. *)
let find_sat_assignment_tests : (formula * truth_assignment option) list = 
  [
    (x, Some xt);
    (!x, Some xf);
    
    (x * x, Some xt);
    (x * !x, None); 
    (x + x, Some xt);
    (x + !x, Some xt);
    
    (x * y, Some xtyt);
    (x * !y, Some xtyf);
    (x + y, Some xtyt);
    (x + y, Some xtyf);
    (x + !y, Some xtyt);
    (x + !y, Some xtyf);
    
    (x * y * z, Some xtytzt);
    (x * y * !z, Some xtytzf);
    (x * !y * z, Some xtyfzt);
    (x * !y * !z, Some xtyfzf);
    (x + y + z, Some xtytzt);
    (x + y + z, Some xtytzf);
    (x + y + z, Some xtyfzf);
    (x + y + z, Some xtyfzt);
    
  ]

(* Question 1 *)
(*----------------------------------------*) 
    

(* TODO: Implement the function. *)
    (*let find_sat_assignment_exc (formula : formula) : truth_assignment =
       let vars = collect_variables formula in
       let rec find rest acc =
         match rest with
         | [] -> 
             if eval acc formula then Some acc
             else None
         | v::vs -> 
             let res = find vs (Variable_map.add v true acc) in
             begin match res with
               | Some res -> Some res
               | None -> find vs (Variable_map.add v false acc)
             end 
       in
       match find vars Variable_map.empty with
       | Some assgn -> assgn
       | None -> raise Unsatisfiable_formula*)
let find_sat_assignment_exc (formula : formula) : truth_assignment =
  let vars = collect_variables formula in
  let rec find rest assgn =
    match rest with
    | [] -> 
        if eval assgn formula then assgn
        else raise Unsatisfiable_formula
    | v::vs -> 
        try find vs (Variable_map.add v true assgn) with 
        | Unsatisfiable_formula -> find vs (Variable_map.add v false assgn) 
  in
  find vars Variable_map.empty
      

(* Question 2 *)
(*----------------------------------------*)


(* TODO: Implement the function. *)
let find_sat_assignment_cps (formula : formula)
    (return : truth_assignment -> 'r) (fail : unit -> 'r) : 'r =
  let vars = collect_variables formula in 
  let rec find rest assgn return fail = 
    match rest with
    | [] -> if eval assgn formula then return assgn else fail () 
    | v::vs -> find vs (Variable_map.add v true assgn) return 
                 (fun () -> find vs (Variable_map.add v false assgn) return fail) 
  in
  find vars Variable_map.empty return fail

