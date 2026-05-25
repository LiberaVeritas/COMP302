(* GRADE:  100% *)
let open_account (pass : password) : bank_account =
  (* TODO: create any helper variables and/or functions *)
  let bal = ref 0 in
  let locked = ref false in
  let pw_cnt = ref 0 in
  
  
  let check_pass p : unit = 
    if not (p = pass) then (incr pw_cnt; raise wrong_pass);
    pw_cnt := 0; 
  in

  let check_neg amt = 
    if amt < 0 then raise negative_amount;
  in
  
  let check_lock () : unit = 
    if !pw_cnt >= 3 then (locked := true; raise account_locked);
    if !locked then raise account_locked;
  in
  
  let check_enough_bal amt =
    if amt > !bal then raise not_enough_balance; 
  in
  
 
  (* TODO: Implement deposit to add money to the account *)
  let deposit p amt = 
    check_lock ();
    check_pass p;
    check_neg amt;
    
    bal := !bal + amt
  in
  (* TODO: Implement show_balance for the account *)
  let show_balance p = 
    check_lock ();
    check_pass p; 
    
    !bal
  in
  (* TODO: Implement withdraw money for the account *)
  let withdraw p amt = 
    check_lock ();
    check_pass p;
    check_neg amt; 
    check_enough_bal amt; 
    
    bal := !bal - amt
  in
  {
    deposit;
    show_balance;
    withdraw;
  }
  

