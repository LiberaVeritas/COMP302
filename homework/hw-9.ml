(* GRADE:  100% *)
(** Substitution & Evaluation *)
let free_vars_test_helper_tests : (exp * ident list) list = [
  (ConstI 5, []);
  (Var "x", ["x"])
]

let rec free_vars (e : exp) : IdentSet.t = 
  match e with
  | ConstI _ -> IdentSet.empty
  | PrimBop (e1, _, e2) -> IdentSet.union (free_vars e1) (free_vars e2)
  | PrimUop (_, e') -> free_vars e'

  | ConstB _ -> IdentSet.empty
  | If (e', e1, e2) -> IdentSet.union (free_vars e') (free_vars e1) |> 
                       IdentSet.union (free_vars e2)

  | Comma (e1, e2) -> IdentSet.union (free_vars e1) (free_vars e2)
  | LetComma (x, y, e1, e2) -> 
      (IdentSet.union (free_vars e1) (free_vars e2)) |>
      IdentSet.remove x |>
      IdentSet.remove y 
                                 
                               

  | Fn (x, tOpt, e') -> IdentSet.remove x (free_vars e')
  | Apply (e1, e2) -> (IdentSet.union (free_vars e1) (free_vars e2))

  | Rec (f, tOpt, e') -> (free_vars e') |> IdentSet.remove f

  | Let (x, e1, e2) -> 
      let res = IdentSet.union (free_vars e2) (free_vars e1) in
      if Var x = e1 then res else res |> IdentSet.remove x
  | Var x -> IdentSet.singleton x
                 
(** DO NOT Change This Definition *)
let free_vars_test_helper e = IdentSet.elements (free_vars e)

let subst_tests : (((exp * ident) * exp) * exp) list = [
  (((ConstI 5, "x"), PrimBop (ConstI 2, Plus, Var "x")), PrimBop (ConstI 2, Plus, ConstI 5))
]
let rev (a, b) = (b, a)
let uncomma e = 
  match e with
  | Comma (a, b) -> (a, b)
  | _ -> raise ParserFailure
           
           
let rec subst ((d, z) : exp * ident) (e : exp) : exp =
  (** [rename (x, e)] replace [x] appears in [e] with a fresh identifier
      and returns the fresh identifier and updated expression *)
  let rename ((x, e) : ident * exp) : ident * exp =
    let x' = fresh_ident x in
    (x', subst (Var x', x) e)
  in
  
  match e with
  | ConstI _ -> e
  | PrimBop (e1, bop, e2) -> PrimBop (subst (d, z) e1, bop, subst (d, z) e2)
  | PrimUop (uop, e') -> PrimUop (uop, subst (d, z) e')

  | ConstB _ -> e
  | If (e', e1, e2) -> If (subst (d, z) e', subst (d, z) e1, subst (d, z) e2)

  | Comma (e1, e2) -> Comma (subst (d, z) e1, subst (d, z) e2)
  | LetComma (x, y, e1, e2) -> 
      let x', e2' = rename (x, e2) in
      let y', e2'' = rename (y, e2') in 
      LetComma (x', y', subst (d, z) e1, subst (d, z) e2'')
          

  | Fn (x, tOpt, e') -> 
      let (x', e'') = rename (x, e') in
      Fn (x', tOpt, subst (d, z) e'')
    
      
  | Apply (e1, e2) -> 
      Apply (subst (d, z) e1, subst (d, z) e2) 

  | Rec (f, tOpt, e') -> 
      let f', e'' = rename (f, e') in 
      Rec (f', tOpt, subst (d, z) e'') 
        
  | Let (x, e1, e2) -> 
      let (x', e2') = rename (x, e2) in 
      Let (x', subst (d, z) e1, subst (d, z) e2')
      
  | Var x ->
      if x = z
      then d
      else e

let eval_test_helper_tests : (exp * exp option) list = [
  (Var "x", None);
  (ConstI 5, Some (ConstI 5));
  (PrimBop (ConstI 5, Minus, ConstI 5), Some (ConstI 0))
]

let rec eval (e : exp) : exp =
  match e with
  | ConstI _ -> e
  | PrimBop (e1, bop, e2) ->
      begin
        match eval e1, eval e2 with
        | ConstI n1, ConstI n2 ->
            begin
              match bop with
              | Equals -> ConstB (n1 = n2)
              | LessThan -> ConstB (n1 < n2)
              | Plus -> ConstI (n1 + n2)
              | Minus -> ConstI (n1 - n2)
              | Times -> ConstI (n1 * n2)
            end
        | _ -> raise EvaluationStuck
      end
  | PrimUop (u, e) ->
      begin
        match eval e with
        | ConstI n -> ConstI (- n)
        | _ -> PrimUop (u, eval e)
      end

  | ConstB _ -> e
  | If (e', e1, e2) -> 
      begin match eval e' with
        | ConstB true -> eval e1
        | ConstB false -> eval e2
        | _ -> If (eval e', eval e1, eval e2)
      end

  | Comma (e1, e2) -> Comma (eval e1, eval e2)
  | LetComma (x, y, e1, e2) -> 
      begin match eval e1 with
        | Comma (f1, f2) -> (subst (f1, x) e2 |> subst (f2, y)) |> eval
        | _ -> LetComma (x, y, eval e1, eval e2)
      end

  | Fn (x, tOpt, e') -> Fn (x, tOpt,  e')
        
      
  | Apply (e1, e2) -> 
      begin match eval e1 with
        | Fn (x, tOpt, e') -> (subst (e2, x) e') |> eval
        | Rec (f, tOpt, e') -> (subst (e2, f) e') |> eval
        | If (_, _, _) -> Apply (eval e1, eval e2)
        | Let (_, _, _) -> Apply (eval e1, eval e2)
        | LetComma (_, _, _, _) -> Apply (eval e1, eval e2) 
        | ConstI _ -> raise EvaluationStuck
        | ConstB _ -> raise EvaluationStuck
        | _ -> e
      end

  | Rec (f, tOpt, e') -> 
      subst (Rec (f, tOpt, e'), f) e' |> eval
        (*
          begin match e' with
            | Fn (x, tOpt, body) -> subst (eval e', f) body |> eval
            | _ -> eval e'
          end*)

  | Let (x, e1, e2) -> subst (e1, x) e2 |> eval
  | Var _ -> raise EvaluationStuck

(** DO NOT Change This Definition *)
let eval_test_helper e =
  try
    Some (eval e)
  with
  | EvaluationStuck -> None
    
    




let ex = PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                  PrimBop (Var "y", Times, Var "y"))


let ex1 : exp =
  Fn ("t", Some (Pair (Int, Int)),
      LetComma ("x", "y", Var "t",
                PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                         PrimBop (Var "y", Times, Var "y"))))
let ex2_string : string = "fn x : int => true"
let ex2 : exp = Fn ("x", Some Int, ConstB true)
let ex3_string : string =
  "let f = fn t : int * int => let (x, y) = t in x * x + (y * y) end in f (3, 4) end"
let ex3 : exp =
  Let ("f",
       Fn ("t", Some (Pair (Int, Int)),
           LetComma ("x", "y", Var "t",
                     PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                              PrimBop (Var "y", Times, Var "y")))),
       Apply (Var "f", Comma (ConstI 3, ConstI 4)))
let ex4_string : string = "let g = (fn x : int => true) in g 0 end"
let ex4 : exp =
  Let ("g", Fn ("x", Some Int, ConstB true), Apply (Var "g", ConstI 0))
let ex5_string : string =
  "let f = fn t : int * int => let (x, y) = t in x * x + (y * y) end in f 3 end"
let ex5 : exp =
  Let ("f",
       Fn ("t", Some (Pair (Int, Int)),
           LetComma ("x", "y", Var "t",
                     PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                              PrimBop (Var "y", Times, Var "y")))),
       Apply (Var "f", ConstI 3))
let ex6_string : string =
  "let f = (fn x : int => (fn y : int => x * x + (y * y))) in (f 3) 4 end"
let ex6 : exp =
  Let ("f",
       Fn ("x", Some Int,
           Fn ("y", Some Int,
               PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                        PrimBop (Var "y", Times, Var "y")))),
       Apply (Apply (Var "f", ConstI 3), ConstI 4))
let ex7_string : string =
  "let fib = let helper = rec h : int * int -> int -> int => fn ab : int * int => let (a, b) = ab in fn steps : int => if steps < 1 then a else h (b, a + b) (steps - 1) end in helper (0, 1) end in fib 9 end"
let ex7 : exp =
  Let ("fib",
       Let ("helper",
            Rec ("h", Some (Arrow (Pair (Int, Int), Arrow (Int, Int))),
                 Fn ("ab", Some (Pair (Int, Int)),
                     LetComma ("a", "b", Var "ab",
                               Fn ("steps", Some Int,
                                   If (PrimBop (Var "steps", LessThan, ConstI 1), Var "a",
                                       Apply
                                         (Apply (Var "h", Comma (Var "b", PrimBop (Var "a", Plus, Var "b"))),
                                          PrimBop (Var "steps", Minus, ConstI 1))))))),
            Apply (Var "helper", Comma (ConstI 0, ConstI 1))),
       Apply (Var "fib", ConstI 9))
    
    

let adv_ex1_string : string = "fn t => let (x, y) = t in x * x + (y * y) end"
let adv_ex1 : exp =
  Fn ("t", None,
      LetComma ("x", "y", Var "t",
                PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                         PrimBop (Var "y", Times, Var "y"))))
let adv_ex2_string : string = "fn x => true"
let adv_ex2 : exp = Fn ("x", None, ConstB true)
let adv_ex3_string : string =
  "let f = fn t => let (x, y) = t in x * x + (y * y) end in f (3, 4) end"
let adv_ex3 : exp =
  Let ("f",
       Fn ("t", Some (Pair (Int, Int)),
           LetComma ("x", "y", Var "t",
                     PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                              PrimBop (Var "y", Times, Var "y")))),
       Apply (Var "f", Comma (ConstI 3, ConstI 4)))
let adv_ex4_string : string = "let g = (fn x => true) in g 0 end"
let adv_ex4 : exp =
  Let ("g", Fn ("x", Some Int, ConstB true), Apply (Var "g", ConstI 0))
let adv_ex5_string : string =
  "let f = fn t => let (x, y) = t in x * x + (y * y) end in f 3 end"
let adv_ex5 : exp =
  Let ("f",
       Fn ("t", Some (Pair (Int, Int)),
           LetComma ("x", "y", Var "t",
                     PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                              PrimBop (Var "y", Times, Var "y")))),
       Apply (Var "f", ConstI 3))
let adv_ex6_string : string =
  "let f = (fn x => (fn y => x * x + (y * y))) in (f 3) 4 end"
let adv_ex6 : exp =
  Let ("f",
       Fn ("x", None,
           Fn ("y", None,
               PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                        PrimBop (Var "y", Times, Var "y")))),
       Apply (Apply (Var "f", ConstI 3), ConstI 4))
let ill_ex1_string : string =
  "let f = (fn x : int => (fn y : bool => x * x + (y * y))) in f (3, 4) end"
let ill_ex1 : exp =
  Let ("f",
       Fn ("x", Some Int,
           Fn ("y", Some Bool,
               PrimBop (PrimBop (Var "x", Times, Var "x"), Plus,
                        PrimBop (Var "y", Times, Var "y")))),
       Apply (Var "f", Comma (ConstI 3, ConstI 4)))

