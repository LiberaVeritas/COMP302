(* GRADE:  100% *)
(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = 
  [
    (0, Z);
    (1, S Z);
    (2, S (S Z));
    (3, S (S (S Z)));
  ]
;;

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let q1a_nat_of_int (n : int) : nat = 
  let rec f (i: int) (acc: nat) =
    if (i = n) then acc
    else f (i + 1) (S acc)
  in
  f 0 Z

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = 
  [
    (Z, 0);
    (S Z, 1);
    (S (S Z), 2);
    (S (S (S Z)), 3);
  ]
;;

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let q1b_int_of_nat (n : nat) : int = 
  let rec f (i: nat) (acc: int) =
    if (i = n) then acc 
    else f (S i) (acc + 1)
  in
  f Z 0

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = 
  [ 
    ( (Z, Z), Z );
    ( (Z, (S Z)), (S Z) );
    ( ((S Z), Z), (S Z) );
    ( ((S (S (S Z))), (S (S Z))), (S (S (S (S (S Z))))));
  ]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat =
  match n, m with
  | Z, m -> m
  | n, Z -> n
  | (S n1), m -> q1c_add n1 (S m)
                   


(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times (Const (-1.0), e)
           

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus (e1, (q2a_neg e2))

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp = 
  let rec f (n: nat) (acc: exp) : exp = 
    match n with 
    | Z -> Const 0.
    | S Z -> acc
    | S n' -> f n' (Times (e1, acc)) 
  in
  f p (Times (e1, Const 1.))


(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = 
  [ 
    ((1., Var), 1.);
    ((1., Times (Var, Const 2.)), 2.);
    ((1., Plus (Var, Const 2.)), 3.);
    ((2., Plus (Const 4., Var)), 6.);
    ((1., Const 5.), 5.);
    ((2., Times (Plus (Var, Const 2.), Div (Const 4., Var))), 8.);
    ((100., Times (Plus (Const 2., Const 1.), Div (Const 4., Const 2.))), 6.);
  ]
;;

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float =
  match e with
  | Const f -> f
  | Var -> a 
  | Plus (e1, e2) -> eval a e1 +. eval a e2 
  | Times (e1, e2) -> eval a e1 *. eval a e2
  | Div (e1, e2) -> eval a e1 /. eval a e2


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = 
  [
    (Var, Const 1.);
    (Const 4., Const 0.);
    (Plus (Var, Const 3.), Plus (Const 1., Const 0.)); 
    (Times (Const (-1.), Var), Plus (Times (Const 0., Var), Times (Const (-1.), Const 1.)));
    (Times (Var, Var), Plus (Times (Const 1., Var), Times (Var, Const 1.)));
    (Div (Const 5., Var), Div (q2b_minus (Times (Const 0., Var)) (Times (Const 5., Const 1.)),
                               (Times (Var, Var))))
  ]

(* TODO: Implement {!diff}. *) 
                                  
   (* D(e1 + e2) = D(e1) + D(e2)
D(e1 * e2) = D(e1) * e2 + e1 * D(e2) (product rule)
D(x) = 1
D(a) = 0 (a is a constant)
D(e1 / e2) = (D(e1) * e2 - e1 * D(e2)) / (e2 * e2) *)
let rec diff (e : exp) : exp = 
  match e with
  | Plus (e1, e2) -> Plus (diff e1, diff e2)
  | Times (e1, e2) -> Plus (Times (diff e1, e2), Times (e1, diff e2))
  | Var -> Const 1.
  | Const _ -> Const 0.
  | Div (e1, e2) -> Div (q2b_minus (Times (diff e1, e2)) (Times (e1, diff e2)),
                         (Times (e2, e2)))
    
    
    
    
    
    
