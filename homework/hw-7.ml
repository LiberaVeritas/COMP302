(* GRADE:  100% *)
(* SECTION 1 *)

(*  Question 1.1 *)
let rec repeat (x : 'a) : 'a stream = 
  {
    head = x;
    tail = Susp (fun () -> repeat x)
  }

(* Question 1.2 *)
let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  let h = s.head in 
  let res = fun () -> filter f (force s.tail) in
  if f h then
    {
      head = h; 
      tail = Susp res 
    }
  else
    let res = res () in
    {
      head = res.head; 
      tail = Susp (fun () -> force res.tail)
    }
    

(* Question 1.3 *)
      (* L(0) = 2
L(1) = 1
L(n) = L(n-1) + L(n-2) *)
let rec lucas1 =
  {
    (* You should fix these *)
    head = 2;
    tail = Susp (fun () -> lucas2);
  }

and lucas2 =
  {
    (* You should fix these *)
    head = 1;
    tail = Susp (fun () -> zip_with ( + ) lucas1 lucas2);
  }

(* Question 1.4 *)
let unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  let g (_, s) = f s in 
  let (a,b) = f seed in
  map fst
    {
      head = (a,b);
      tail = Susp (fun () -> (iterate g (f b)))
    }


(* Question 1.5 *)
let unfold_lucas : int stream = 
  let l0 = 2 in
  let l1 = 1 in
  let f (ln_1, ln_2) = (ln_1 + ln_2, (ln_1 + ln_2, ln_1)) in
  { head = 2; tail = Susp (fun () -> 
        { head = 1; tail = Susp (fun () -> unfold f (l1, l0))})}

(* SECTION 2 *)

(* Question 2.1 *)
let scale (s1 : int stream) (n : int) : int stream = 
  map (( * ) n) s1

(* Question 2.2 *)
    (*let s = 
       let ones = repeat 1 in
       let nats = iterate (fun x -> (x + 1)) 1 in
       let pow2 = iterate (fun x -> (x * 2)) 1 in
       let pow3 = iterate (fun x -> (x * 3)) 1 in
       let pow5 = iterate (fun x -> (x * 5)) 1 in
       let f (n, s) = (n + 1, (merge (scale s (exp 2 n)) (merge (scale s (3 ** n)) (scale s (5 ** n))))) in
       unfold f (1, ones) ;*)
  
    (*let s = 
       let big = {head = 99999; tail = Susp (fun () -> raise NotImplemented)} in
       let one = { head = 1; tail = Susp (fun () -> big )} in
       let _ = repeat 1 in
       let f (s) = 
         let m = 
           (scale s 2) |> merge
             (scale s 3) |> merge
             (scale s 5) 
         in 
         (s.head, m)
       in
       unfold f s*)
let rec s = 
  
  {head = 1; tail = Susp (fun () -> (scale s 2)
                                    |> merge (scale s 3)
                                    |> merge (scale s 5))}
    (*

nats 1 2 3 4 5 ... n

scale 2^n, 3^n 5^n

*)


