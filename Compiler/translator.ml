open S
open G

exception Not_implemented 

let tmp_index = ref 0
let label_index = ref 1
let new_temp() = tmp_index := !tmp_index + 1; ".t" ^ (string_of_int !tmp_index)
let new_label() = label_index := !label_index + 1; !label_index

(*************************************)
(*          translation to T         *)
(*************************************)
 
let s2t : S.program -> T.program = fun (decls, stmts) ->
  (* reset counters *)
  tmp_index   := 0;
  label_index := 1;

  (* cur_lbl always holds the label we’re currently emitting into *)
  let curr_lbl = ref (new_label ()) in

  (* code is built backwards, then reversed at the end *)
  let code = ref [] in

  (* switch to a new basic‐block label *)
  let start_block l =
    curr_lbl := l;
    (* record an explicit SKIP at the head of each block *)
    code := (l, T.SKIP) :: !code
  in

  (* emit one instruction under `!curr_lbl` *)
  let emit instr =
    code := (!curr_lbl, instr) :: !code
  in

  (* begin with an initial block *)
  start_block !curr_lbl;

  (* 1) compile the declarations as zero‐inits and allocs *)
  List.iter (fun (typ, x) ->
    match typ with
    | S.TINT   ->
      emit (T.COPYC (x, 0))
    | S.TARR n ->
      emit (T.ALLOC (x, n))
  ) decls;

  (* forward‐declare mutually recursive functions *)
  let rec trans_exp (e : S.exp) : T.instr list * T.var =
    match e with
    | S.NUM n ->
      let v = new_temp () in
      ([T.COPYC (v, n)], v)

    | S.LV (S.ID x) ->
      let v = new_temp () in
      ([T.COPY (v, x)], v)

    | S.LV (S.ARR (x, idx_e)) ->
      let (cidx, vidx) = trans_exp idx_e in
      let v = new_temp () in
      (cidx @ [T.LOAD (v, (x, vidx))], v)

    | ( S.ADD _ | S.SUB _ | S.MUL _ | S.DIV _
      | S.LT  _ | S.LE  _ | S.GT  _ | S.GE  _
      | S.EQ  _ | S.AND _ | S.OR  _) as bop_e ->
      let bop = match bop_e with
        | S.ADD _ -> T.ADD  | S.SUB _ -> T.SUB
        | S.MUL _ -> T.MUL  | S.DIV _ -> T.DIV
        | S.LT  _ -> T.LT   | S.LE  _ -> T.LE
        | S.GT  _ -> T.GT   | S.GE  _ -> T.GE
        | S.EQ  _ -> T.EQ   | S.AND _ -> T.AND
        | S.OR  _ -> T.OR   | _ -> assert false
      in
      let (c1, v1) = trans_exp (match bop_e with S.ADD(a,_) -> a
                                         | S.SUB(a,_) -> a
                                         | S.MUL(a,_) -> a
                                         | S.DIV(a,_) -> a
                                         | S.LT (a,_) -> a
                                         | S.LE (a,_) -> a
                                         | S.GT (a,_) -> a
                                         | S.GE (a,_) -> a
                                         | S.EQ (a,_) -> a
                                         | S.AND(a,_) -> a
                                         | S.OR (a,_) -> a
                                         | _ -> assert false) in
      let (c2, v2) = trans_exp (match bop_e with S.ADD(_,b) -> b
                                         | S.SUB(_,b) -> b
                                         | S.MUL(_,b) -> b
                                         | S.DIV(_,b) -> b
                                         | S.LT (_,b) -> b
                                         | S.LE (_,b) -> b
                                         | S.GT (_,b) -> b
                                         | S.GE (_,b) -> b
                                         | S.EQ (_,b) -> b
                                         | S.AND(_,b) -> b
                                         | S.OR (_,b) -> b
                                         | _ -> assert false) in
      let v = new_temp () in
      (c1 @ c2 @ [T.ASSIGNV (v, bop, v1, v2)], v)

    | S.MINUS x ->
      let (c, v1) = trans_exp x in
      let v = new_temp () in
      (c @ [T.ASSIGNU (v, T.MINUS, v1)], v)

    | S.NOT x ->
      let (c, v1) = trans_exp x in
      let v = new_temp () in
      (c @ [T.ASSIGNU (v, T.NOT, v1)], v)
  in

  let rec trans_stmt (st : S.stmt) : unit =
    match st with

    (* assignment to variable or array *)
    | S.ASSIGN (lv, e) ->
      let (pre, rv) = trans_exp e in
      List.iter emit pre;
      begin match lv with
      | S.ID x ->
        emit (T.COPY (x, rv))
      | S.ARR (x, idx_e) ->
        let (ci, vidx) = trans_exp idx_e in
        List.iter emit ci;
        emit (T.STORE ((x, vidx), rv))
      end

    (* I/O *)
    | S.READ  x ->
      emit (T.READ x)
    | S.PRINT e ->
      let (pre, rv) = trans_exp e in
      List.iter emit pre;
      emit (T.WRITE rv)

    (* if/then/else *)
    | S.IF (cond, tbr, fbr) ->
      let (pc, rv) = trans_exp cond in
      List.iter emit pc;

      let l_then = new_label () in
      let l_else = new_label () in
      let l_join = new_label () in

      (* branch into then or else *)
      emit (T.CJUMP (rv, l_then));
      emit (T.UJUMP l_else);

      (* then block *)
      start_block l_then;
      trans_stmt tbr;
      emit (T.UJUMP l_join);

      (* else block *)
      start_block l_else;
      trans_stmt fbr;
      emit (T.UJUMP l_join);

      (* join block *)
      start_block l_join

    (* while *)
    | S.WHILE (cond, body) ->
      let l_head = new_label () in
      let l_body = new_label () in
      let l_join = new_label () in

      (* head: test *)
      start_block l_head;
      let (pc, rv) = trans_exp cond in
      List.iter emit pc;
      emit (T.CJUMPF (rv, l_join));

      (* body *)
      start_block l_body;
      trans_stmt body;
      emit (T.UJUMP l_head);

      (* exit *)
      start_block l_join

    (* do‐while *)
    | S.DOWHILE (body, cond) ->
      let l_body = new_label () in
      let l_test = new_label () in

      (* always execute body first *)
      start_block l_body;
      trans_stmt body;

      (* then test *)
      start_block l_test;
      let (pc, rv) = trans_exp cond in
      List.iter emit pc;
      emit (T.CJUMP (rv, l_body))

    (* block *)
    | S.BLOCK (_, ss) ->
      List.iter trans_stmt ss
  in

  (* translate all the top‐level statements *)
  List.iter trans_stmt stmts;

  (* finally, halt *)
  emit T.HALT;

  (* reverse to get increasing label order *)
  List.rev !code

(*************************************)
(*     translation from S to Cfg     *)
(*************************************)

let s2cfg : S.program -> Cfg.t = fun (decls, stmts) ->

  tmp_index := 0;
  label_index := 1;

  let add_node (cfg, preds) node =
    let cfg' = Cfg.add_node node cfg in
    let cfg'' = List.fold_left (fun g p -> Cfg.add_edge p node g) cfg' preds in
    (cfg'', [node])
  in

  let rec build_decls (cfg, preds) decls =
  match decls with
  | [] -> (cfg, preds)
  | (typ, x) :: rest ->
    let (cfg', ps) =
      match typ with
      | TARR n ->
          (* array decl still becomes an ALLOC node *)
          add_node (cfg, preds) (Node.create_alloc x n)
      | TINT   ->
          (* integer decl now becomes “x = 0” *)
          let init = Node.create_assign (S.ID x) (S.NUM 0) in
          add_node (cfg, preds) init
    in
    build_decls (cfg', ps) rest

  and build_stmts state stmts =
    match stmts with
    | [] -> state
    | s :: ss ->
      let state' = build_stmt state s in
      build_stmts state' ss

  and build_stmt (cfg, preds) stmt =
    match stmt with
    | ASSIGN (lv, e) ->
      add_node (cfg, preds) (Node.create_assign lv e)

    | READ x ->
      add_node (cfg, preds) (Node.create_read x)

    | PRINT e ->
      add_node (cfg, preds) (Node.create_print e)

    | BLOCK (ds, ss) ->
      let state1 = build_decls (cfg, preds) ds in
      build_stmts state1 ss

    | IF (e, s1, s2) ->
      let (cfg1, ps_then1) = add_node (cfg, preds) (Node.create_assume e) in
      let n_then = List.hd ps_then1 in
      let (cfg2, ps_then2) = build_stmt (cfg1, [n_then]) s1 in

      let (cfg3, ps_else1) = add_node (cfg2, preds) (Node.create_assume (NOT e)) in
      let n_else = List.hd ps_else1 in
      let (cfg4, ps_else2) = build_stmt (cfg3, [n_else]) s2 in

      (cfg4, ps_then2 @ ps_else2)

    | WHILE (e, body) ->
      let (cfg1, ps_split) = add_node (cfg, preds) (Node.create_skip ()) in
      let n_split = List.hd ps_split in

      (* 2) create the ASSUME‐true from n_split, *then* mark THAT as the loop‐head: *)
      let (cfg2, ps_true) = add_node (cfg1, [n_split]) (Node.create_assume e) in
      let n_true = List.hd ps_true in
      let cfg2 = Cfg.add_loophead n_true cfg2 in

      (* 3) build the body off of n_true, hook it back… *)
      let (cfg3, ps_body) = build_stmt (cfg2, [n_true]) body in
      let cfg4 =
        List.fold_left (fun g p -> Cfg.add_edge p n_true g) cfg3 ps_body
      in

      (* 4) create the ASSUME‐false back on n_split, return that as the “exit” *)
      let (cfg5, ps_false) =
        add_node (cfg4, [n_split]) (Node.create_assume (NOT e))
      in
      let n_false = List.hd ps_false in
      (cfg5, [n_false])

    | DOWHILE (body, e) ->
      let (cfg1, ps_body1) = build_stmt (cfg, preds) body in

      let (cfg2, ps_split) = add_node (cfg1, ps_body1) (Node.create_skip ()) in
      let n_split = List.hd ps_split in
      let cfg2 = Cfg.add_loophead n_split cfg2 in

      let (cfg3, ps_true) = add_node (cfg2, [n_split]) (Node.create_assume e) in
      let n_true = List.hd ps_true in
      let (cfg4, ps_body2) = build_stmt (cfg3, [n_true]) body in
      let cfg5 = List.fold_left (fun g p -> Cfg.add_edge p n_split g) cfg4 ps_body2 in

      let (cfg6, ps_false) = add_node (cfg5, [n_split]) (Node.create_assume (NOT e)) in
      let n_false = List.hd ps_false in
      (cfg6, [n_false])
  in

  let entry = Node.create_skip () in
  let cfg0 = Cfg.add_node entry Cfg.empty in
  let preds0 = [entry] in

  let (cfg1, preds1) =
    let st_decls = build_decls (cfg0, preds0) decls in
    build_stmts st_decls stmts
  in

  let exit_node = Node.create_skip () in
  let (cfg2, _) = add_node (cfg1, preds1) exit_node in

  let all_nodes = Cfg.nodesof cfg2 in
  let cfg3 = List.fold_left (fun g n ->
    if n <> exit_node && NodeSet.is_empty (Cfg.succs n g) then
      Cfg.add_edge n exit_node g
    else g
  ) cfg2 all_nodes in

  cfg3