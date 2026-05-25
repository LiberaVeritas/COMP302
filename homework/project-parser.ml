(* GRADE:  100% *)
(** Exercise 1: Parse a Tree *)
let parse_tree_tests : (string * tree option) list = [
  ("", None);
  ("((. < 5 > .) < 4 > .)", Some (Node (Node (Leaf, 5, Leaf), 4, Leaf)))
]

open Parser
let leaf' = const_map Leaf (accept_char '.')
let btw_space p = between spaces spaces p
let leaf = btw_space leaf'
    
let lbracket' = accept_char '<'
let lbracket = btw_space lbracket'
    
let rbracket' = accept_char '>'
let rbracket = btw_space rbracket'
    
let lparen' = accept_char '('
let lparen = btw_space lparen'
    
let rparen' = accept_char ')'
let rparen = btw_space rparen'
    
let pass s = fun v -> const_map v s
    
let btw_bracket p = lbracket |>> p |*> pass rbracket
  
let value = btw_bracket int_digits
    
let btw_paren p = lparen |>> p |*> pass rparen
  
  
let rec tree_parser i =
  let open Parser in 
  let tmap = 
    (map3 (fun l v r -> Node (l, v, r))
       (tree_parser)
       (value)
       (tree_parser))
  in
  let tp = btw_paren tmap in
  let tree_parser_impl = 
    first_of_2 leaf tp
  in
  tree_parser_impl i

(** DO NOT Change This Definition *)
let parse_tree : string -> tree option =
  let open Parser in
  run (between spaces eof tree_parser)

(** Part 1: Parse an Arithmetic Expression *)
let parse_arith_tests : (string * arith option) list = [
  (*("", None);*)
  ("5 ^ 4", Some (Bop (Const 5, Power, Const 4)));
  ("5 - 4 ", Some (Bop (Const 5, Minus, Const 4)));
  ("(5 ^ 4 ) ", Some (Bop (Const 5, Power, Const 4)));
  ("(4)", Some (Const 4)); 
  ("8 ^ (5 ^ 2)", Some (Bop (Const 8, Power, (Bop (Const 5, Power, Const 2)))));
]

let plus' = accept_char '+' |> const_map Plus
let plus = btw_space plus'
    
let minus' = accept_char '-' |> const_map Minus
let minus = btw_space minus'
    
let times' = accept_char '*' |> const_map Times
let times = btw_space times'
    
let power' = accept_char '^' |> const_map Power
let power = btw_space power'
    
let bmap x bop y = map3 (fun x b y -> Bop (x, b, y)) x bop y
    
let num = btw_space (map (fun i -> Const i) int_digits)
    
let plusminus = first_of_2 plus minus
    
let fbop a b c = Bop (a, b, c)

let rec arith_parser i =
  let open Parser in
  let atomic_exp_parser =
    (** You may need to use [arith_parser] here *) 
    first_of_2 
      (btw_paren arith_parser)
      num
      
  in
  let power_exp_parser =
    first_of_2 
      (right_assoc_op power atomic_exp_parser fbop)
      atomic_exp_parser


  in
  let multiplicative_exp_parser =
    first_of_2
      (left_assoc_op times power_exp_parser fbop)
      power_exp_parser


  in
  let arith_exp_parser_impl =
    first_of_2
      (left_assoc_op plusminus multiplicative_exp_parser fbop)
      multiplicative_exp_parser 

  in 
  arith_exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_arith : string -> arith option =
  let open Parser in
  run (between spaces eof arith_parser)

