open Regex
open Nfa

exception Not_implemented

let or_nfa nfa1 nfa2 =
  
  let base = create_new_nfa () in
  let base_initial = get_initial_state base in

  let (final_state, base) = create_state base in

  let base = add_states base (get_states nfa1) in
  let base = add_states base (get_states nfa2) in

  let (edges1, eps1) = get_edges nfa1 in
  let (edges2, eps2) = get_edges nfa2 in
  let base = add_edges base (edges1, eps1) in
  let base = add_edges base (edges2, eps2) in

  let base = add_epsilon_edge base (base_initial, get_initial_state nfa1) in
  let base = add_epsilon_edge base (base_initial, get_initial_state nfa2) in

  let base =
    BatSet.fold (fun st acc -> add_epsilon_edge acc (st, final_state))
      (get_final_states nfa1)
      base
  in
  let base =
    BatSet.fold (fun st acc -> add_epsilon_edge acc (st, final_state))
      (get_final_states nfa2)
      base
  in

  add_final_state base final_state

let concat_nfa nfa1 nfa2 =
  let base = create_new_nfa () in
  
  let base = add_states base (get_states nfa1) in
  let base = add_states base (get_states nfa2) in
  let base_initial = get_initial_state base in
  
  let base = add_epsilon_edge base (base_initial, get_initial_state nfa1) in
  let (edges1, eps1) = get_edges nfa1 in
  let (edges2, eps2) = get_edges nfa2 in
  let base = add_edges base (edges1, eps1) in
  let base = add_edges base (edges2, eps2) in

  let base =
    BatSet.fold (fun st acc -> add_epsilon_edge acc (st, get_initial_state nfa2))
      (get_final_states nfa1)
      base
  in

  let base =
    BatSet.fold (fun st acc -> add_final_state acc st)
      (get_final_states nfa2)
      base
  in
  base

let star_nfa nfa =
  let base = create_new_nfa () in
  let base_initial = get_initial_state base in
  let (final_state, base) = create_state base in

  let base = add_states base (get_states nfa) in
  let (edges, eps) = get_edges nfa in
  let base = add_edges base (edges, eps) in

  let base = add_epsilon_edge base (base_initial, get_initial_state nfa) in
  let base = add_epsilon_edge base (base_initial, final_state) in

  let base =
    BatSet.fold (fun st acc -> add_epsilon_edge acc (st, get_initial_state nfa))
      (get_final_states nfa)
      base
  in
  let base =
    BatSet.fold (fun st acc -> add_epsilon_edge acc (st, final_state))
      (get_final_states nfa)
      base
  in

  add_final_state base final_state

let rec regex2nfa re =
  match re with
  | Empty ->
      create_new_nfa ()
  | Epsilon ->
      let nfa = create_new_nfa () in
      let s = get_initial_state nfa in
      add_final_state nfa s
  | Alpha c ->
      let nfa = create_new_nfa () in
      let s1 = get_initial_state nfa in
      let (s2, nfa) = create_state nfa in
      let nfa = add_edge nfa (s1, c, s2) in
      add_final_state nfa s2
  | OR (r1, r2) ->
      or_nfa (regex2nfa r1) (regex2nfa r2)
  | CONCAT (r1, r2) ->
      concat_nfa (regex2nfa r1) (regex2nfa r2)
  | STAR r ->
      star_nfa (regex2nfa r)


let epsilon_closure (nfa : Nfa.t) (ss : Nfa.states) : Nfa.states =
  let visited = ref ss in
  let queue = Queue.create () in
  BatSet.iter (fun s -> Queue.push s queue) ss;
  while not (Queue.is_empty queue) do
    let s = Queue.pop queue in
    let e_next = Nfa.get_next_state_epsilon nfa s in
    let new_states = BatSet.diff e_next !visited in
    if not (BatSet.is_empty new_states) then begin
      visited := BatSet.union !visited new_states;
      BatSet.iter (fun st -> Queue.push st queue) new_states;
    end
  done;
  !visited

let move (nfa : Nfa.t) (ss : Nfa.states) (sym : alphabet) : Nfa.states =
  BatSet.fold (fun s acc ->
    let next = Nfa.get_next_state nfa s sym in
    BatSet.union next acc
  ) ss BatSet.empty

  let nfa2dfa (nfa : Nfa.t) : Dfa.t =
    let init_nfa_state = Nfa.get_initial_state nfa in
    let init_subset = epsilon_closure nfa (BatSet.singleton init_nfa_state) in
  
    let dfa_ref = ref (Dfa.create_new_dfa init_subset) in
  
    let nfa_finals = Nfa.get_final_states nfa in
    if not (BatSet.is_empty (BatSet.intersect init_subset nfa_finals)) then
      dfa_ref := Dfa.add_final_state !dfa_ref init_subset;
  
    let discovered = ref (BatSet.singleton init_subset) in
    let queue = Queue.create () in
    Queue.push init_subset queue;
  
    let dead_state = BatSet.empty in
    let dead_added = ref false in
  
    while not (Queue.is_empty queue) do
      let curr_subset = Queue.pop queue in
      List.iter (fun sym ->
        let next_no_eps = move nfa curr_subset sym in
        let next_subset = epsilon_closure nfa next_no_eps in
        if BatSet.is_empty next_subset then
          begin
            if not !dead_added then begin
              dfa_ref := Dfa.add_state !dfa_ref dead_state;
              dead_added := true;
              dfa_ref := Dfa.add_edge !dfa_ref (dead_state, A, dead_state);
              dfa_ref := Dfa.add_edge !dfa_ref (dead_state, B, dead_state);
            end;
            dfa_ref := Dfa.add_edge !dfa_ref (curr_subset, sym, dead_state)
          end
        else begin
          if not (BatSet.mem next_subset !discovered) then begin
            discovered := BatSet.add next_subset !discovered;
            dfa_ref := Dfa.add_state !dfa_ref next_subset;
            if not (BatSet.is_empty (BatSet.intersect next_subset nfa_finals)) then
              dfa_ref := Dfa.add_final_state !dfa_ref next_subset;
            Queue.push next_subset queue;
          end;
          dfa_ref := Dfa.add_edge !dfa_ref (curr_subset, sym, next_subset);
        end
      ) [A; B];
    done;
    !dfa_ref
 
let regex2dfa : Regex.t -> Dfa.t
=fun regex -> 
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
    dfa


let run_dfa (dfa : Dfa.t) (input : alphabet list) : bool
=let start_state = Dfa.get_initial_state dfa in
  
let rec process current_state symbols =
  match symbols with
  | [] ->
     Dfa.is_final_state dfa current_state
  | sym :: rest ->
     let next_state = Dfa.get_next_state dfa current_state sym in
     process next_state rest
in

process start_state input
