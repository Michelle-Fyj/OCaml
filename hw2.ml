let rec subset a b =
  match a with
    [] -> true
    | h::t -> if(List.mem h b) then subset t b
    else false;;


let equal_sets a b =
  subset a b && subset b a;;

let rec computed_fixed_point eq f x =
  if (eq x (f x)) then x
  else computed_fixed_point eq f (f x);;



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

(* end of filtering *)



let rec convert_rules_to_function list_of_rules =
function nonterminal_symbol ->
match list_of_rules with
[] -> []
| h::t -> if (fst h) = nonterminal_symbol then (snd h)::(convert_rules_to_function t nonterminal_symbol)
else convert_rules_to_function t nonterminal_symbol


let convert_grammar gram1 = 
    let good_grammar = filter_blind_alleys gram1 in
    (fst good_grammar, convert_rules_to_function(snd good_grammar))



let rec parse_prefix_helper start_symbol grammar list_of_possible_symbols_from_start accept derivation frag =
  match list_of_possible_symbols_from_start with
  | [] -> None
  | h :: t ->
    match find_path grammar h accept (derivation @ [(start_symbol, h)]) frag with
    | None -> parse_prefix_helper start_symbol grammar t accept derivation frag
    | Some result -> Some result


and find_path grammar rules accept derivation frag =
  match rules with
  | [] -> accept derivation frag
  | (T ter) :: rest_tail1 -> ( match frag with [] -> None | h :: t -> if ter = h then find_path grammar rest_tail1 accept derivation t else None )
  | (N nonter) :: rest_tail2 -> parse_prefix_helper nonter grammar (grammar nonter) (find_path grammar rest_tail2 accept) derivation frag


let parse_prefix (start_symbol, grammar) accept frag = parse_prefix_helper start_symbol grammar (grammar start_symbol) accept [] frag
