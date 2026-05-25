(* GRADE:  100% *)
(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("", None);
  ("5", Some (ConstI 5));
  ("true", Some (ConstB true));
  ("(x)", Some (Var "x"));
  ("f x", Some (Apply (Var "f", Var "x")));
  ("(f x)", Some (Apply (Var "f", Var "x")));
  ("fn x : int -> int => x 2", (Some (Fn ("x", Some (Arrow (Int, Int)), Apply (Var "x", ConstI 2)))));
  ("fn y : int * bool => true", Some (Fn ("y", (Some (Pair (Int, Bool))), ConstB true)));
  ("fn x => fn y => 7", Some (Fn ("x", None, Fn ("y", None, ConstI 7)))); 
  ("if true then 2 else 3, 2", (Some (If (ConstB true, ConstI 2, Comma (ConstI 3, ConstI 2)))));
]

open Parser

let btw_space p = between spaces spaces p
let pass s = fun v -> const_map v s
    
let lparen = accept_char '(' |> btw_space 
let rparen = accept_char ')' |> btw_space 
let btw_paren p = lparen |>> p |*> pass rparen
                               
let num = int_digits |> btw_space |>
          map (fun i -> ConstI i)
            
let comma = accept_char ',' |> btw_space
let colon = accept_char ':' |> btw_space
                               
                               
let negate = accept_char '-' |> const_map Negate
let equals = accept_char '=' |> btw_space |> const_map Equals
let less = accept_char '<' |> btw_space |> const_map LessThan
let star = accept_char '*' |> btw_space
                  
                  
let s_arrow = accept_string "->" |> btw_space
let d_arrow = accept_string "=>" |> btw_space
            
            
let tru = accept_string "true" |> btw_space |> const_map (ConstB true)
let fls = accept_string "false" |> btw_space |> const_map (ConstB false)
let bool = first_of_2 tru fls
  
    
let int_t = accept_string "int" |> btw_space |> const_map Int 
let bool_t = accept_string "bool" |> btw_space |> const_map Bool 
               


    

let plus = accept_char '+' |> btw_space |> const_map Plus 
let minus = accept_char '-' |> btw_space |> const_map Minus 
let times = accept_char '*' |> btw_space |> const_map Times 
let space = symbol " "
              
    
              
let pair_op = (fun t1 t2 -> Pair (t1, t2))
let arrow_op = (fun t1 t2 -> Arrow (t1, t2))
  
let pair a b = map2 pair_op a (star |>> b)
let arrow a b = map2 arrow_op a (s_arrow |>> b)
    

let apply_op = (fun e1 _ e2 -> Apply (e1, e2))
let apply = map2 apply_op
    
let bmap = map3 (fun l b r -> PrimBop (l, b, r)) 
let fbop a b c = PrimBop (a, b, c)
    
let if_op a b c = If (a, b, c)
let comma_op a _ c = Comma (a, c)
    
let let_op i e1 e2 = Let (i, e1, e2)
let let_comma_op i1 i2 e1 e2 = LetComma (i1, i2, e1, e2)
let fn_op i t e = Fn (i, t, e)
let rec_op i t e = Rec (i, t, e)
let var_op v = Var v
  
    
  
let map4 f p q r s = p |*> fun a -> map3 (f a) q r s
  
  
let fuop a b = PrimUop (a, b) 
  
    
let plusminus = first_of_2 plus minus
let leq = first_of_2 less equals
  
    
let fbop_of p a b = map3 fbop a p b
  
                          
    
    (*let fbop a b c = Bop (a, b, c)*)
    

let rec exp_parser i =
  let open Parser in
  (** Use [identifier] and [keyword] in your implementation,
      not [identifier_except] and [keyword_among] directly *)
  let identifier, keyword =
    let keywords = ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
    identifier_except keywords, keyword_among keywords
  in
  let id = identifier |> map var_op |> btw_space in
  
  let atomic_exp = 
    first_of 
      [
        num;
        bool;
        id;
        btw_paren exp_parser;
      ]
  in
  let rec typ i =
    let atomic_typ i = 
      let atomic = first_of 
          [ 
            int_t; 
            bool_t; 
            (typ |> btw_paren)
          ] 
      in
      atomic i
    in
    let pair_typ = 
      first_of_2
        (pair atomic_typ atomic_typ)
        atomic_typ
    in
    let typ_parser =
      first_of_2
        (arrow pair_typ typ)
        pair_typ
    in 
    typ_parser i
  in
  (** You may need to define helper parsers depending on [exp_parser] here *)
  let applicative_exp =
    first_of_2 
      (left_assoc_op (of_value ()) atomic_exp apply_op)
      atomic_exp
  in 
  
  let let_exp = 
    map3 let_op 
      (keyword "let" |>> identifier) 
      (equals |>> exp_parser)
      (between (keyword "in") (keyword "end") exp_parser)
  in
  
  let let_comma_exp =
    map4 let_comma_op
      (keyword "let" |>> lparen |>> identifier)
      (comma |>> identifier)
      (rparen |>> equals |>> exp_parser)
      (between (keyword "in") (keyword "end") exp_parser)
  in
  
  let if_exp =
    map3 if_op
      (keyword "if" |>> exp_parser)
      (keyword "then" |>> exp_parser)
      (keyword "else" |>> exp_parser)
  in
  
  let fn_exp = 
    map3 fn_op
      (keyword "fn" |>> identifier)
      (colon |>> typ |> optional)
      (d_arrow |>> exp_parser) 
  in
  
  let rec_exp =
    map3 rec_op
      (keyword "rec" |>> identifier)
      (colon |>> typ |> optional)
      (d_arrow |>> exp_parser)
  in
  
  let negatable_exp = 
    first_of
      [
        let_comma_exp;
        let_exp;
        if_exp;
        fn_exp;
        rec_exp;
        applicative_exp;
      ]
  in
  
  let negation_exp = 
    (prefix_op negate negatable_exp fuop) 
  in
  
  let multiplicative_exp =
    first_of_2
      (left_assoc_op times negation_exp fbop)
      negation_exp
  in
  
  let additive_exp =
    first_of_2
      (left_assoc_op plusminus multiplicative_exp fbop)
      multiplicative_exp
  in
  
  let comparative_exp =
    first_of_2
      (non_assoc_op leq additive_exp fbop)
      additive_exp
  in 
  
  let exp_parser_impl =
    first_of_2 
      (non_assoc_op comma comparative_exp comma_op)
      comparative_exp
  in 
  
  exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)

