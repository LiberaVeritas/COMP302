(* GRADE:  100% *)
(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for testing your tree depth function. *)
(* For the whole assignment, you only need to write tests for `int tree`s
   etc. However, your functions must still be suitably polymorphic, and the
   grader will check this. *)
let t0 = Empty
let t1 = Tree (t0, 1, t0)
let t2 = Tree (t1, 2, t0)
let t2' = Tree (t0, 2, t1)
let t2'' = Tree (t1, 2, t1)
let t3 = Tree (t2, 3, t0)
let t3' = Tree (t2, 3, t2)
let t3'' = Tree (t2'', 3, t2')
  
let tree_depth_cps_test_cases : (int tree * int) list = [
  (t0, 0);
  (t1, 1);
  (t2, 2);
  (t2', 2);
  (t2'', 2);
  (t3, 3);
  (t3', 3);
  (t3'', 3);
]

(* These are the test cases that will actually be graded, but
   you don't have to modify this. Remember that you only need
   to test with the `id` continuation. `insert_test_continuations`
   (defined in the prelude) adds the `id` continuation to each of
   your test cases. *)
let tree_depth_cps_tests : ((int tree * (int -> int)) * int) list =
  insert_test_continuations tree_depth_cps_test_cases 
  

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth (t : 'a tree) =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

let id = fun x -> x
(* TODO: Implement a CPS style tree_depth_cps function.*)
let rec tree_depth_cps (t : 'a tree) (return : int -> 'r) : 'r = 
  match t with
  | Empty -> return 0
  | Tree (l, _, r) -> 
      tree_depth_cps l (fun left ->
          (tree_depth_cps r (fun right -> 
               maxk left right (fun n -> return (1 + n)))))
        

           

(* Question 2: Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *)
let traverse_cps_test_cases : (int tree * int list) list = [
  (t0, []);
  (t1, [1]);
  (t2, [2;1]);
  (t2', [2;1]);
  (t2'', [2;1;1]);
  (t3, [3;2;1]);
  (t3', [3;2;1;2;1]);
  (t3'', [3;2;1;1;2;1]);
];;
let traverse_cps_tests : ((int tree * (int list -> int list)) * int list) list =
  insert_test_continuations traverse_cps_test_cases

(* An example of non-CPS function to preorder traverse a tree *)
let rec tree_traverse (t : 'a tree) = 
  match t with
  | Empty -> []
  | Tree (l, x, r) -> x :: tree_traverse l @ tree_traverse r

(* TODO: Implement a CPS style preorder traversal function. *)
let rec traverse_cps (t : 'a tree) (return : 'a list -> 'r) : 'r = 
  match t with
  | Empty -> return []
  | Tree (l, x, r) -> 
      traverse_cps l (fun left -> 
          traverse_cps r (fun right -> return (x :: left @ right)))
        

(* Question 3: Max Elements in a Tree *)
(* TODO: Write a good set of tests for testing your tree maximum function. *)
let tree_max_cps_test_cases : (int tree * int) list = [
  (t0, (-1));
  (t1, 1);
  (t2, 2);
  (t2', 2);
  (t2'', 2);
  (t3, 3);
  (t3', 3);
  (t3'', 3);
  ((Tree (Empty, 73,
          Tree (Tree (Empty, 77, Empty), 24, Tree (Empty, 57, Empty)))), 77);
  ((Tree (Empty, 68, Empty)), 68);
  ((Tree (Tree (Empty, 46, Tree (Empty, 93, Empty)), 81,
          Tree (Empty, 89, Empty))), 93);

];;
let tree_max_cps_tests : ((int tree * (int -> int)) * int) list =
  insert_test_continuations tree_max_cps_test_cases

(* TODO: Implement a CPS style tree maximum function. *)
let rec tree_max_cps (t : int tree) (return : int -> 'r) : 'r = 
  match t with
  | Empty -> return (-1)
  | Tree (l, x, r) ->
      tree_max_cps l (fun left ->
          tree_max_cps r (fun right->
              maxk left right (fun n -> maxk x n return)))







