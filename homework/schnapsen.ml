(* GRADE:  100% *)
let point_value = function
  | Num 10 -> 10
  | Ace -> 11 
  | King -> 4
  | Queen -> 3
  | Jack -> 2
  | Num n  -> failwith ("rank " ^ string_of_int n ^ " does not exist in Schnapsen!")
  (* You need to add some more cases here. *)

(* You should use `match` to take apart the `card`s, but an if-else
   chain will probably be helpful for the actual logic. *)
let who_wins (trumps : suit) (lead : card) (follow : card) : winner =
  match lead, follow with
  | (suit1, _), (suit2, _) when suit1 = trumps && suit2 != trumps -> Lead
  | (suit1, _), (suit2, _) when suit1 != trumps && suit2 = trumps -> Follow
  | (suit1, rank1), (suit2, rank2) when suit1 = suit2 -> 
      if (point_value rank1 >= point_value rank2) then Lead
      else Follow
  | _ -> Lead
        

