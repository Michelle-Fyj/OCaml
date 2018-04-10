let rec subset a b =
  match a with
    [] -> true
    | h::t -> if(List.mem h b) then subset t b
    else false;;


let equal_sets a b =
  subset a b && subset b a;;


let rec set_union a b =
  match a with
  	[] -> b
  	| h::t -> if(List.mem h b) then set_union t b
  				else h::(set_union t b);;


let rec set_intersection a b =
  match a with
  	[] -> []
  	| h::t -> if(List.mem h b) then h::(set_intersection t b)
  				else set_intersection t b;;

let rec set_diff a b =
  match a with
  [] -> []
  | h::t -> if(List.mem h b) then set_diff t b
  			else h::(set_diff t b);;


let rec computed_fixed_point eq f x =
  if (eq x (f x)) then x
  else computed_fixed_point eq f (f x);;



let rec compute_p_times f p x =
  if (p = 0) then x
  else compute_p_times f (p-1) (f x);;

let rec computed_periodic_point eq f p x =
  if (eq x (compute_p_times f p x)) then x
  else computed_periodic_point eq f p (f x);;



let rec while_away s p x =
  if (p x)=false then []
  else x::(while_away s p (s x));;


let rec repeat a b=
  if a = 0 then []
  else b::(repeat (a-1) b)


let rec rle_decode lp =
  match lp with
  	[] -> []
  	| h::t -> repeat (fst h) (snd h) @ rle_decode t;;



(*filter_blind_alleys g*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


let isTerminal tuple_x =
		match tuple_x with
	N _ -> false
	| T _ -> true

let rec allTerminal l = 
		match l with
	| [] -> true
	| h::t -> if (isTerminal h) then (allTerminal t) 
				else false

let rec findT l = 
		match l with
	| [] -> []
	| h::t -> if (allTerminal (snd h)) then (fst h)::(findT t) 
						else findT t

let rec filter l = 
		match l with
	| [] -> []
	| (N a)::t -> a::filter t
	| (T a)::t -> filter t

let rec listGoodN l list_of_GoodN =
	match l with
		| [] -> list_of_GoodN
		| (non,list_of_sym) :: t -> if (subset (filter list_of_sym) list_of_GoodN)
					  then (non) :: (listGoodN t (list_of_GoodN))
					  else listGoodN t list_of_GoodN


let rec filter_work list_of_Rules list_of_GoodN =
	match list_of_Rules with
		| [] -> []
		| (non,syms_list) :: t -> if (subset (filter syms_list) list_of_GoodN)
					  then (non,syms_list) :: (filter_work t list_of_GoodN)
					  else filter_work t list_of_GoodN


let filter_blind_alleys g = 
	let list_of_GoodN = 
	computed_fixed_point equal_sets (listGoodN (snd g)) (findT (snd g)) in
	(fst g , filter_work (snd g) list_of_GoodN)
