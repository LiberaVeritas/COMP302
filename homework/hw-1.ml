(* GRADE:  100% *)
(* helpers *)

(*
(* list of naturals from 1 to n *)
  let nats (n: int) : int list = 
    let rec nats' (i: int) (acc: int list) : int list = 
      if (i = n) then acc @ [i]
      else nats' (i + 1) (acc @ [i])
    in
    nats' 1 [] 
  ;;

(* factorial 
int -> int *)
  let factorial = function
    | 0 -> 1 
    | n -> List.fold_left ( * ) 1 (nats n)
  ;;

(* alternate factorial; without lib functions or helper 
int -> int *)
  let factorial_alt (n: int) : int =
    let rec factorial_alt' (i: int) (acc: int) : int =
      if (i = n) then acc * n
      else factorial_alt' (i + 1) (acc * i)
    in
    factorial_alt' 1 1
  ;;
*)
  
    
    

(* Question 1: Manhattan Distance *)

let distance_tests = [
  (
    ((0, 0), (0, 0)), (* input: two inputs, each a pair, so we have a pair of pairs *)
    0                 (* output: the distance between (0,0) and (0,0) is 0 *)
  );                    (* end each case with a semicolon *)
    (* Your test cases go here *)
  (
    ((1, 1), (0, 0)), (* symmetric *)
    2
  );
  (
    ((0, 0), (1, 1)),
    2
  );
  (
    ((-1, 0), (0, 0)), (* non negative *)
    1
  );
  (
    ((0, 0), (0, -1)),
    1
  );
  (
    ((3, 0), (-2, 0)), (* opposite sign *)
    5
  );
  (
    ((0, -2), (0, -3)), (* same sign *)
    1
  );
]
;;

(* L1 norm (manhattan) distance. 
Sum of absolute diffs in each dimension *)
let distance ((x1, y1): int * int) ((x2, y2): int * int) : int = 
  abs (x1 - x2) + abs (y1 - y2) 



(* Question 2: Binomial *)
(* we assume that  n >= k >= 0; *)
let binomial_tests = [ 
  ((0, 0), 1);
  ((5, 0), 1);
  ((5, 1), 5);
  ((5, 5), 1);
  ((10, 5), 252);

]
;; 

(* factorial *)
let factorial (n: int) : int =
  if (n = 0) then 1 else
    let rec factorial' (i: int) (acc: int) : int =
      if (i = n) then acc * n
      else factorial' (i + 1) (acc * i)
    in 
    factorial' 1 1
;;
(* Binomial (n choose k) := n! / (k! (n - k)!) *) 
let binomial n k =
  (factorial n) / ((factorial k) * (factorial (n - k)))
;;



(* Question 3: Lucas Numbers 
L(0) = 2
L(1) = 1
L(n) = L(n-1) + L(n-2)
*)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
  (0, 2);
  (1, 1);
  (2, 3);
  (3, 4);
  (10, 123);
  (25, 167761);
]

(* Compute lucas nums up to n steps keeping acc tuple (L(n-1), L(n-2))
and return L(n-1) + L(n-2) *) 

let rec lucas_helper (n: int) (step: int) ((ln_1, ln_2): int * int) : int =
  if (step = n) then (ln_1 + ln_2)
  else lucas_helper n (step + 1) ((ln_1 + ln_2), ln_1)
;;
    (* build up from 0, 1 to n, acc is tuple last two *)


(* TODO: Implement lucas by calling lucas_helper. *)
let lucas = function
  | 0 -> 2
  | 1 -> 1
  | n -> lucas_helper n 2 (1, 2)

