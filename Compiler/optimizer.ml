open T

module VarMap = Map.Make(String)

type const_val =
  | Const of int
  | Unknown

let eval_bop (op : bop) (c1 : int) (c2 : int) : int =
  match op with
  | ADD -> c1 + c2
  | SUB -> c1 - c2
  | MUL -> c1 * c2
  | DIV -> if c2 = 0 then 0 else c1 / c2
  | LT  -> if c1 <  c2 then 1 else 0
  | LE  -> if c1 <= c2 then 1 else 0
  | GT  -> if c1 >  c2 then 1 else 0
  | GE  -> if c1 >= c2 then 1 else 0
  | EQ  -> if c1 =  c2 then 1 else 0
  | AND -> if c1 <> 0 && c2 <> 0 then 1 else 0
  | OR  -> if c1 <> 0 || c2 <> 0 then 1 else 0

let eval_uop (op : uop) (c : int) : int =
  match op with
  | MINUS -> -c
  | NOT   -> if c = 0 then 1 else 0

let propagate_and_fold (prog : program) : program =
  let constants : const_val VarMap.t ref = ref VarMap.empty in

  let fold_one ((lbl, instr) : linstr) : linstr =
    match instr with
    | COPYC (x, n) ->
        constants := VarMap.add x (Const n) !constants;
        (lbl, COPYC (x, n))

    | COPY (x, y) ->
        begin match VarMap.find_opt y !constants with
        | Some (Const c) ->
            constants := VarMap.add x (Const c) !constants;
            (lbl, COPYC (x, c))
        | _ ->
            constants := VarMap.add x Unknown !constants;
            (lbl, COPY (x, y))
        end

    | ASSIGNV (x, op, y, z) ->
        let maybe_c1 =
          match VarMap.find_opt y !constants with
          | Some (Const c1) -> Some c1
          | _ -> None
        in
        let maybe_c2 =
          match VarMap.find_opt z !constants with
          | Some (Const c2) -> Some c2
          | _ -> None
        in
        begin match (maybe_c1, maybe_c2) with
        | (Some c1, Some c2) ->
            let folded = eval_bop op c1 c2 in
            constants := VarMap.add x (Const folded) !constants;
            (lbl, COPYC (x, folded))

        | _ ->
            constants := VarMap.add x Unknown !constants;
            (lbl, ASSIGNV (x, op, y, z))
        end

    | ASSIGNC (x, op, y, n) ->
        begin match VarMap.find_opt y !constants with
        | Some (Const c1) ->
            let folded = eval_bop op c1 n in
            constants := VarMap.add x (Const folded) !constants;
            (lbl, COPYC (x, folded))
        | _ ->
            constants := VarMap.add x Unknown !constants;
            (lbl, ASSIGNC (x, op, y, n))
        end

    | ASSIGNU (x, op, y) ->
        begin match VarMap.find_opt y !constants with
        | Some (Const c1) ->
            let folded = eval_uop op c1 in
            constants := VarMap.add x (Const folded) !constants;
            (lbl, COPYC (x, folded))
        | _ ->
            constants := VarMap.add x Unknown !constants;
            (lbl, ASSIGNU (x, op, y))
        end

    | ALLOC (x, n) ->
        constants := VarMap.add x Unknown !constants;
        (lbl, ALLOC (x, n))

    | LOAD (x, (arr_name, idx_var)) ->
        constants := VarMap.add x Unknown !constants;
        (lbl, LOAD (x, (arr_name, idx_var)))

    | STORE ((arr_name, idx_var), src_var) ->
        (lbl, STORE ((arr_name, idx_var), src_var))

    | READ x ->
        constants := VarMap.add x Unknown !constants;
        (lbl, READ x)

    | WRITE x ->
        (lbl, WRITE x)

    | HALT ->
        (lbl, HALT)
    | SKIP ->
        (lbl, SKIP)
    | UJUMP l ->
        (lbl, UJUMP l)

    | CJUMP (x, target_lbl) ->
    begin match VarMap.find_opt x !constants with
    | Some (Const c) ->
        if c <> 0 then
          (lbl, UJUMP target_lbl)
        else
          (lbl, SKIP)

    | Some Unknown
    | None ->
        (lbl, CJUMP (x, target_lbl))
    end

    | CJUMPF (x, target_lbl) ->
    begin match VarMap.find_opt x !constants with
    | Some (Const c) ->
        if c = 0 then
          (lbl, UJUMP target_lbl)
        else
          (lbl, SKIP)

    | Some Unknown
    | None ->
        (lbl, CJUMPF (x, target_lbl))
    end
  in

  List.map fold_one prog

let eliminate_dead (prog : program) : program =
  let use_counts : int VarMap.t ref = ref VarMap.empty in
  let incr_use v =
    let prev = Option.value ~default:0 (VarMap.find_opt v !use_counts) in
    use_counts := VarMap.add v (prev + 1) !use_counts
  in

  let scan_instr (_lbl, instr) : unit =
    let use_of_var v = incr_use v in
    match instr with
    | COPYC _            -> ()
    | COPY (_x, y)       -> use_of_var y
    | ASSIGNV (_x, _op, y, z) ->
        use_of_var y; use_of_var z
    | ASSIGNC (_x, _op, y, _n) ->
        use_of_var y
    | ASSIGNU (_x, _op, y) ->
        use_of_var y
    | ALLOC _            -> ()
    | LOAD (_x, (arr_name, idx_var)) ->
        use_of_var arr_name;
        use_of_var idx_var
    | STORE ((arr_name, idx_var), src_var) ->
        use_of_var arr_name;
        use_of_var idx_var;
        use_of_var src_var
    | READ _             -> ()
    | WRITE x            -> use_of_var x
    | UJUMP _            -> ()
    | CJUMP (x, _)       -> use_of_var x
    | CJUMPF (x, _)      -> use_of_var x
    | SKIP               -> ()
    | HALT               -> ()
  in

  List.iter scan_instr prog;

  let keep_or_kill_entry ((lbl, instr) : linstr) : linstr =
    let use_of_var v =
      let prev = Option.value ~default:0 (VarMap.find_opt v !use_counts) in
      if prev > 0 then
        use_counts := VarMap.add v (prev - 1) !use_counts
    in

    match instr with
    | ASSIGNV (x, op, y, z) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else begin
          use_of_var y;
          use_of_var z;
          (lbl, ASSIGNV (x, op, y, z))
        end

    | ASSIGNC (x, op, y, n) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else begin
          use_of_var y;
          (lbl, ASSIGNC (x, op, y, n))
        end

    | ASSIGNU (x, op, y) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else begin
          use_of_var y;
          (lbl, ASSIGNU (x, op, y))
        end

    | COPY (x, y) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else begin
          use_of_var y;
          (lbl, COPY (x, y))
        end

    | COPYC (x, n) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else
          (lbl, COPYC (x, n))

    | ALLOC (x, n) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else
          (lbl, ALLOC (x, n))

    | LOAD (x, (arr_name, idx_var)) ->
        let uses_of_x = Option.value ~default:0 (VarMap.find_opt x !use_counts) in
        if uses_of_x = 0 then
          (lbl, SKIP)
        else begin
          use_of_var arr_name;
          use_of_var idx_var;
          (lbl, LOAD (x, (arr_name, idx_var)))
        end

    | STORE ((arr_name, idx_var), src_var) ->
        use_of_var arr_name;
        use_of_var idx_var;
        use_of_var src_var;
        (lbl, STORE ((arr_name, idx_var), src_var))

    | READ x ->
        (lbl, READ x)

    | WRITE x ->
        use_of_var x;
        (lbl, WRITE x)

    | CJUMP (x, target_lbl) ->
        use_of_var x;
        (lbl, CJUMP (x, target_lbl))

    | CJUMPF (x, target_lbl) ->
        use_of_var x;
        (lbl, CJUMPF (x, target_lbl))

    | UJUMP l ->
        (lbl, UJUMP l)
    | HALT ->
        (lbl, HALT)
    | SKIP ->
        (lbl, SKIP)
  in

  List.map keep_or_kill_entry prog


let optimize (pgm : program) : program =
  let folded = propagate_and_fold pgm in
  let dced   = eliminate_dead folded   in
  List.filter (fun (_lbl, instr) -> instr <> SKIP) dced