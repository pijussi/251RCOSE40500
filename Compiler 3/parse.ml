open Util
open Batteries

type symbol = T of string | N of string | Epsilon | End
type production = symbol * symbol list
type cfg = symbol list * symbol list * symbol * production list

let string_of_symbol s = 
  match s with 
  | T x -> x 
  | N x -> x 
  | Epsilon -> "epsilon"
  | End -> "$"

let string_of_prod (lhs, rhs) = 
    string_of_symbol lhs ^ " -> " ^ 
      string_of_list ~first:"" ~last:"" ~sep:" " string_of_symbol rhs 

module FIRST = struct 
  type t = (symbol, symbol BatSet.t) BatMap.t

  let empty = BatMap.empty 
  
  let find : symbol -> t -> symbol BatSet.t 
  =fun s t -> try BatMap.find s t with _ -> BatSet.empty 
  
  let add : symbol -> symbol -> t -> t 
  =fun s1 s2 t -> BatMap.add s1 (BatSet.add s2 (find s1 t)) t
  
  let add_set : symbol -> symbol BatSet.t -> t -> t 
  =fun s1 ss t -> BatSet.fold (fun s2 -> add s1 s2) ss t

  let tostring : t -> string
  =fun t -> 
    BatMap.foldi (fun s ss str -> 
      str ^ string_of_symbol s ^ " |-> " ^ string_of_set string_of_symbol ss ^ "\n"
    ) t ""
end   

module FOLLOW = struct
  type t = (symbol, symbol BatSet.t) BatMap.t 

  let empty = BatMap.empty 

  let find : symbol -> t -> symbol BatSet.t 
  =fun s t -> try BatMap.find s t with _ -> BatSet.empty 

  let add : symbol -> symbol -> t -> t 
  =fun s1 s2 t -> BatMap.add s1 (BatSet.add s2 (find s1 t)) t

  let add_set : symbol -> symbol BatSet.t -> t -> t 
  =fun s1 ss t -> BatSet.fold (fun s2 -> add s1 s2) ss t

  let tostring : t -> string
  =fun t -> 
    BatMap.foldi (fun s ss str -> 
      str ^ string_of_symbol s ^ " |-> " ^ string_of_set string_of_symbol ss ^ "\n"
    ) t ""
end

module ParsingTable = struct
  type t = (symbol * symbol, (symbol * symbol list) BatSet.t) BatMap.t 

  let empty = BatMap.empty 

  let find (nonterm, term) t = try BatMap.find (nonterm, term) t with _ -> BatSet.empty 

  let add (nonterm, term) prod t = 
    BatMap.add (nonterm, term) (BatSet.add prod (find (nonterm, term) t)) t
    
  let tostring : t -> string 
  =fun t -> 
    BatMap.foldi (fun (nonterm, term) prods str -> 
      str ^ string_of_symbol nonterm ^ ", " ^ string_of_symbol term ^ " |-> " ^
        string_of_set string_of_prod prods ^ "\n"
    ) t ""
    
  let foldi = BatMap.foldi 

  let for_all = BatMap.for_all
end

let compute_first (grammar : cfg) : FIRST.t =
  let (nonterms, terms, _, prods) = grammar in
  let init =
    List.fold_left (fun acc sym ->
      match sym with
      | T _ | End -> FIRST.add sym sym acc
      | Epsilon -> FIRST.add sym sym acc
      | N _ -> BatMap.add sym BatSet.empty acc
    ) FIRST.empty (nonterms @ terms @ [Epsilon; End])
  in
  let rec fix first_map =
    let changed = ref false in
    let new_map =
      List.fold_left (fun fmap (lhs, rhs) ->
        let current = FIRST.find lhs fmap in
        let rec first_seq lst =
          match lst with
          | [] -> BatSet.singleton Epsilon
          | x::xs ->
              let fx = FIRST.find x fmap in
              if BatSet.mem Epsilon fx then
                BatSet.union (BatSet.remove Epsilon fx) (first_seq xs)
              else
                BatSet.remove Epsilon fx
        in
        let computed = first_seq rhs in
        let unioned = BatSet.union current computed in
        if not (BatSet.equal current unioned) then begin
          changed := true;
          BatMap.add lhs unioned fmap
        end else fmap
      ) first_map prods
    in
    if !changed then fix new_map else new_map
  in
  fix init

let compute_follow (grammar : cfg) (first_map : FIRST.t) : FOLLOW.t =
  let (nonterms, _, start, prods) = grammar in
  let init =
    List.fold_left (fun acc sym ->
      match sym with
      | N _ -> BatMap.add sym BatSet.empty acc
      | _ -> acc
    ) FOLLOW.empty nonterms
  in
  let init = FOLLOW.add start End init in
  let rec fix follow_map =
    let changed = ref false in
    let new_map =
      List.fold_left (fun fmap (lhs, rhs) ->
        List.fold_left (fun fmap (i, sym) ->
          match sym with
          | N _ ->
            let beta = if i+1 < List.length rhs then drop (i+1) rhs else [] in
              let rec first_seq lst =
                match lst with
                | [] -> BatSet.singleton Epsilon
                | x::xs ->
                    let fx = FIRST.find x first_map in
                    if BatSet.mem Epsilon fx then
                      BatSet.union (BatSet.remove Epsilon fx) (first_seq xs)
                    else
                      BatSet.remove Epsilon fx
              in
              let first_beta = first_seq beta in
              let current_follow = FOLLOW.find sym fmap in
              let new_follow = BatSet.union current_follow first_beta in
              let beta_derives_epsilon =
                let rec all_epsilon lst =
                  match lst with
                  | [] -> true
                  | x::xs -> BatSet.mem Epsilon (FIRST.find x first_map) && all_epsilon xs
                in all_epsilon beta
              in
              let new_follow =
                if beta_derives_epsilon then BatSet.union new_follow (FOLLOW.find lhs fmap)
                else new_follow
              in
              if not (BatSet.equal current_follow new_follow) then begin
                changed := true;
                FOLLOW.add_set sym new_follow fmap
              end else fmap
          | _ -> fmap
        ) fmap (List.mapi (fun i sym -> (i, sym)) rhs)
      ) follow_map prods
    in
    if !changed then fix new_map else new_map
  in
  fix init

let rec derives_epsilon (rhs : symbol list) (first_map : FIRST.t) : bool =
  match rhs with
  | [] -> true
  | x :: xs ->
      BatSet.mem Epsilon (FIRST.find x first_map) && derives_epsilon xs first_map

let first_of_seq (rhs : symbol list) (first_map : FIRST.t) : symbol BatSet.t =
  let rec aux lst =
    match lst with
    | [] -> BatSet.singleton Epsilon
    | x :: xs ->
        let fx = FIRST.find x first_map in
        if BatSet.mem Epsilon fx then
          BatSet.union (BatSet.remove Epsilon fx) (aux xs)
        else
          BatSet.remove Epsilon fx
  in
  aux rhs

  let check_LL1 (grammar : cfg) : bool =
    let (_nonterms, _, _, prods) = grammar in
    let first_map = compute_first grammar in
    let follow_map = compute_follow grammar first_map in
  
    let groups =
      List.fold_left (fun acc prod ->
        let lhs, _ = prod in
        let group = try BatMap.find lhs acc with Not_found -> [] in
        BatMap.add lhs (prod :: group) acc
      ) BatMap.empty prods
    in
  
    let predictive_set (lhs, rhs) =
      let first_rhs = first_of_seq rhs first_map in
      if derives_epsilon rhs first_map then
        BatSet.union first_rhs (FOLLOW.find lhs follow_map)
      else
        first_rhs
    in
  
    let group_ok group =
      let preds = List.map predictive_set group in
      let rec check lst =
        match lst with
        | [] -> true
        | x :: xs ->
            List.for_all (fun y -> BatSet.is_empty (BatSet.intersect x y)) xs && check xs
      in
      check preds
    in
  
    BatMap.foldi
      (fun _ group acc ->
         acc && group_ok group
      )
      groups
      true
  
module TableKey = struct
  type t = symbol * symbol
  let compare = compare
end
module Table = BatMap.Make(TableKey)

let build_parsing_table (grammar : cfg) (first_map : FIRST.t) (follow_map : FOLLOW.t) : production Table.t =
  let table = ref Table.empty in
  let (_, _, _, prods) = grammar in
  List.iter (fun (lhs, rhs) ->
    let first_rhs = first_of_seq rhs first_map in
    let predictive =
      if BatSet.mem Epsilon first_rhs then
        BatSet.union (BatSet.remove Epsilon first_rhs) (FOLLOW.find lhs follow_map)
      else
        first_rhs
    in
    BatSet.iter (fun terminal ->
      match terminal with
      | T _ | End ->
          table := Table.add (lhs, terminal) (lhs, rhs) !table
      | Epsilon | N _ -> ()
    ) predictive
  ) prods;
  !table

let parse (grammar : cfg) (input : symbol list) : bool =
  let first_map = compute_first grammar in
  let follow_map = compute_follow grammar first_map in
  let table = build_parsing_table grammar first_map follow_map in
  let (_, _, start, _) = grammar in

  let stack = Stack.create () in
  Stack.push End stack;
  Stack.push start stack;

  let input_ref = ref input in

  let rec loop () =
    if Stack.is_empty stack then
      !input_ref = []  
    else
      let top = Stack.pop stack in
      match top with
      | (T _ | End) as terminal ->
          (match !input_ref with
           | [] -> false
           | x :: xs ->
               if x = terminal then (input_ref := xs; loop ())
               else false)
      | N _ as nonterminal ->
          (match !input_ref with
           | [] -> false
           | lookahead :: _ ->
               let key = (nonterminal, lookahead) in
               (try
                  let (_, rhs) = Table.find key table in
                  List.iter (fun sym -> if sym <> Epsilon then Stack.push sym stack) (List.rev rhs);
                  loop ()
                with Not_found ->
                  false))
      | Epsilon ->
          loop ()
  in

  loop ()
