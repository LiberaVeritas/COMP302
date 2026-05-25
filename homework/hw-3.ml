(* GRADE:  100% *)
(* Hi everyone. All of these problems are generally "one-liners" and have slick solutions. They're quite cute to think
   about but are certainly confusing without the appropriate time and experience that you devote towards reasoning about
   this style. Good luck! :-) *)

(* For example, if you wanted to use the encoding of five in your test cases, you could define: *)
let five : 'b church = fun s z -> s (s (s (s (s z))))
(* and use 'five' like a constant. You could also just use
   'fun z s -> s (s (s (s (s z))))' directly in the test cases too. *)

(* If you define a personal helper function like int_to_church, use it for your test cases, and see things break, you should
   suspect it and consider hard coding the input cases instead *)

(*---------------------------------------------------------------*)
(* QUESTION 1 *)
let two = fun s z -> s (s z)
let three = fun s z -> s (s (s z))
let four = fun s z -> s (s (s (s z)))
let six : 'b church = fun s z -> s (s (s (s (s (s z)))))
let seven : 'b church = fun s z -> s (s (s (s (s (s (s z))))))

(* Question 1a: Church numeral to integer *)
(* TODO: Test cases *)
let to_int_tests : (int church * int) list = 
  [
    (five, 5);
  ]

(* TODO: Implement:
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems. *)
let to_int (n : int church) : int = 
  n (( + ) 1) 0

(* Question 1b: Determine if a church numeral is zero *)
(* TODO: Test cases *)
let is_zero_tests : ('b church * bool) list = 
  [
    (zero, true);
    (one, false);
    (five, false);
  ]

(* TODO: Implement *)
let is_zero (n : 'b church) : bool =
  n (fun x -> false) true

(* Question 1c: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : (('b church * 'b church) * 'b church) list = 
  [
    ((zero, zero), zero);
    ((zero, one), one);
    ((one, zero), one); 
  ]

(* TODO: Implement *)
let add (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun s z -> n1 s (n2 s z) 

(*---------------------------------------------------------------*)
(* QUESTION 2 *)

(* Question 2a: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : (('b church * 'b church) * 'b church) list = 
  [
    ((zero, zero), zero);
    ((zero, one), zero);
    ((one, zero), zero);
    ((five, one), five);
    ((two, two), four);
    ((three, two), six);
  ]

(* TODO: Implement *)
let mult (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun s -> n1 (n2 s)

(* Question 2b: Compute the power of a church numeral given an int as the power *)
(* TODO: Test cases *)
let int_pow_church_tests : ((int * 'b church) * int) list = 
  [
    ((0, zero), 1);
    ((1, zero), 1);
    ((0, one), 0);
    ((2, two), 4);
    ((2, three), 8);
    ((4, six), 4096);
    ((10, one), 10);
    (((-2), two), 4);
    (((-2), three), (-8));
    ((2, fun s -> three (two s)), 64);
  ]

(* TODO: Implement *)
let int_pow_church (x : int) (n : 'b church) : int = 
  n (( * ) x) 1

