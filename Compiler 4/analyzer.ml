open G 

exception NotImplemented

module type Interval = sig
  type integer = PlusInf | MinusInf | Int of int 
  type t = Bot | Range of integer * integer 
  val bot : t
  val top : t 
  val one : t 
  val zero : t
  val cmp_int : integer -> integer -> int
  val min_int : integer -> integer -> integer
  val max_int : integer -> integer -> integer 
  val from_int : int -> t 
  val from_bounds : integer -> integer -> t
  val order : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val eq : t -> t -> t
  val not : t -> t
  val le : t -> t -> t
  val lt : t -> t -> t 
  val ge : t -> t -> t 
  val gt : t -> t -> t 
  val band : t -> t -> t
  val bor : t -> t -> t
  val to_string : t -> string
end

module Interval : Interval = struct
  type integer = PlusInf | MinusInf | Int of int 
  type t = Bot | Range of integer * integer 
  let bot = Bot 
  let top = Range (MinusInf, PlusInf)
  let one = Range (Int 1, Int 1)
  let zero = Range (Int 0, Int 0)
  let string_of_integer i = 
    match i with
    | PlusInf -> "+oo"
    | MinusInf -> "-oo"
    | Int n -> string_of_int n 
  let to_string i = 
    match i with 
    | Bot -> "Bot"
    | Range (i1, i2) -> "[" ^ string_of_integer i1 ^ ", " ^ string_of_integer i2 ^ "]"
  let from_int n = Range (Int n, Int n)
  let from_bounds i1 i2 = Range (i1, i2)

  let cmp_int i1 i2 = match i1, i2 with
    | MinusInf, MinusInf -> 0
    | MinusInf, _        -> -1
    | _, MinusInf        -> 1
    | PlusInf, PlusInf   -> 0
    | PlusInf, _         -> 1
    | _, PlusInf         -> -1
    | Int x, Int y       -> Stdlib.compare x y

  let min_int a b = if cmp_int a b <= 0 then a else b
  let max_int a b = if cmp_int a b >= 0 then a else b

  let order x y = match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | Range (l1, u1), Range (l2, u2)
      -> cmp_int l2 l1 <= 0 && cmp_int u1 u2 <= 0

  let join a b = match a, b with
    | Bot, x | x, Bot -> x
    | Range (l1, u1), Range (l2, u2)
      -> Range (min_int l1 l2, max_int u1 u2)

  let meet a b = match a, b with
    | Bot, _ | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let lo = max_int l1 l2 in
      let hi = min_int u1 u2 in
      if cmp_int lo hi <= 0 then Range (lo, hi) else Bot

  let widen prev next = match prev, next with
    | Bot, x | x, Bot -> x
    | Range (l1, u1), Range (l2, u2) ->
      let lo = if cmp_int l2 l1 < 0 then MinusInf else l1 in
      let hi = if cmp_int u2 u1 > 0 then PlusInf  else u1 in
      Range (lo, hi)

  let narrow prev next = match prev, next with
    | Bot, _ | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let lo = if cmp_int l2 l1 > 0 then l2 else l1 in
      let hi = if cmp_int u2 u1 < 0 then u2 else u1 in
      if cmp_int lo hi <= 0 then Range (lo, hi) else Bot

  let add x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let lo = match l1, l2 with
        | MinusInf, _ | _, MinusInf -> MinusInf
        | PlusInf, _ | _, PlusInf   -> PlusInf
        | Int a, Int b              -> Int (a + b)
      and hi = match u1, u2 with
        | MinusInf, _ | _, MinusInf -> MinusInf
        | PlusInf, _ | _, PlusInf   -> PlusInf
        | Int a, Int b              -> Int (a + b)
      in Range (lo, hi)
  
  let sub x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let lo = match l1, u2 with
        | MinusInf, _ | _, PlusInf   -> MinusInf
        | PlusInf, _ | _, MinusInf   -> PlusInf
        | Int a, Int b               -> Int (a - b)
      and hi = match u1, l2 with
        | MinusInf, _ | _, PlusInf   -> MinusInf
        | PlusInf, _ | _, MinusInf   -> PlusInf
        | Int a, Int b               -> Int (a - b)
      in Range (lo, hi)
      
  let mul x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let candidates = [ (l1, l2); (l1, u2); (u1, l2); (u1, u2) ]
        |> List.map (fun (a, b) ->
            match a, b with
            | Int i, Int j -> Int (i * j)
            | Int 0, _ | _, Int 0 -> Int 0
            | PlusInf, Int k | Int k, PlusInf ->
                if k < 0 then MinusInf
                else if k > 0 then PlusInf
                else Int 0
            | MinusInf, Int k | Int k, MinusInf ->
                if k < 0 then PlusInf
                else if k > 0 then MinusInf
                else Int 0
            | PlusInf, PlusInf | MinusInf, MinusInf -> PlusInf
            | PlusInf, MinusInf | MinusInf, PlusInf -> MinusInf
        ) in
      let lo = List.fold_left min_int PlusInf candidates in
      let hi = List.fold_left max_int MinusInf candidates in
      Range (lo, hi)

  let div x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | _, Range (l2, u2) when cmp_int l2 (Int 0) <= 0 && cmp_int (Int 0) u2 <= 0 -> top
    | Range (l1, u1), Range (l2, u2) ->
      let quotients =
        [ (l1, l2); (l1, u2); (u1, l2); (u1, u2) ]
        |> List.fold_left (fun acc (a, b) ->
             let q = match a, b with
               | Int i, Int j -> Int (i / j)
               | PlusInf, Int k | Int k, PlusInf ->
                   if k > 0 then PlusInf
                   else if k < 0 then MinusInf
                   else Int 0
               | MinusInf, Int k | Int k, MinusInf ->
                   if k > 0 then MinusInf
                   else if k < 0 then PlusInf
                   else Int 0
               | _ -> PlusInf
             in q :: acc
           ) [] in
      let lo = List.fold_left min_int PlusInf quotients in
      let hi = List.fold_left max_int MinusInf quotients in
      Range (lo, hi)
  
  let bool_of_pred always_true always_false =
    if always_true then one
    else if always_false then zero
    else Range (Int 0, Int 1)
      
  let eq  a b = match a, b with
    | Bot, _ | _, Bot -> Bot
    | Range (l1,u1), Range (l2,u2) ->
      let possible_true  = cmp_int l1 u2 <= 0 && cmp_int u1 l2 >= 0 in
      let possible_false = cmp_int u1 l2 < 0 || cmp_int l1 u2 > 0 in
      bool_of_pred possible_true possible_false

  let le a b = match a, b with
    | Bot, _ | _, Bot -> Bot
    | Range (l1,u1), Range (l2,u2) ->
      bool_of_pred (cmp_int u1 l2 <= 0) (cmp_int l1 u2 > 0)

  let lt a b = match a, b with
    | Bot, _ | _, Bot -> Bot
    | Range (l1,u1), Range (l2,u2) ->
      bool_of_pred (cmp_int u1 l2 < 0) (cmp_int l1 u2 >= 0)

  let ge a b = le b a

  let gt a b = lt b a

  let not = function
    | Bot -> Bot
    | Range (Int 0, Int 0) -> one
    | Range (Int 1, Int 1) -> zero
    | Range (Int 0, Int 1) -> Range (Int 0, Int 1)
    | _ -> Range (Int 0, Int 1)

  let band x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if cmp_int l1 (Int 0) >= 0 && cmp_int l2 (Int 0) >= 0 then
        let hi = match min_int u1 u2 with
          | Int v -> Int v
          | _ -> Int 0
        in Range (Int 0, hi)
      else top

  let bor x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | _ -> join x y
end

type allocsite = int 
let string_of_allocsite l = "l" ^ string_of_int l

module AbsLoc = struct
  type t = Var of string | Allocsite of allocsite 
  let from_var x = Var x 
  let from_allocsite a = Allocsite a 
  let to_string a = 
    match a with 
    | Var x -> x 
    | Allocsite l -> string_of_allocsite l  
end 

module AbsArray = struct
  type t = allocsite BatSet.t * Interval.t 
  let bot = (BatSet.empty, Interval.bot)
  let get_allocsites (a, _) = a
  let get_size (_, sz) = sz
  let create l n = (BatSet.singleton l, Interval.from_int n)
  let join (a1, sz1) (a2, sz2) = (BatSet.union a1 a2, Interval.join sz1 sz2)
  let widen (a1, sz1) (a2, sz2) = (BatSet.union a1 a2, Interval.widen sz1 sz2)
  let narrow (a1, sz1) (a2, sz2) = (BatSet.intersect a1 a2, Interval.narrow sz1 sz2)
  let order (a1, sz1) (a2, sz2) = BatSet.subset a1 a2 && Interval.order sz1 sz2
  let to_string : t -> string 
  =fun (a, sz) -> Printf.sprintf "(%s, %s)" (Utils.string_of_set string_of_allocsite a) (Interval.to_string sz)
end 

module AbsVal = struct
  type t = Interval.t * AbsArray.t
  let bot = (Interval.bot, AbsArray.bot)
  let from_itv itv = (itv, AbsArray.bot)
  let from_absarr a = (Interval.bot, a)
  let get_interval (i, _) = i 
  let get_absarray (_, a) = a
  let join (i1, a1) (i2, a2) = (Interval.join i1 i2, AbsArray.join a1 a2)
  let meet_itv itv (i, a) = (Interval.meet itv i, a)
  let widen (i1, a1) (i2, a2) = (Interval.widen i1 i2, AbsArray.widen a1 a2)
  let narrow (i1, a1) (i2, a2) = (Interval.narrow i1 i2, AbsArray.narrow a1 a2)
  let order (i1, a1) (i2, a2) = Interval.order i1 i2 && AbsArray.order a1 a2
  let add (i1, a1) (i2, a2) = (Interval.add i1 i2, AbsArray.join a1 a2)
  let mul (i1, a1) (i2, a2) = (Interval.mul i1 i2, AbsArray.join a1 a2)
  let sub (i1, a1) (i2, a2) = (Interval.sub i1 i2, AbsArray.join a1 a2)
  let div (i1, a1) (i2, a2) = (Interval.div i1 i2, AbsArray.join a1 a2)
  let le (i1, a1) (i2, a2) = (Interval.le i1 i2, AbsArray.join a1 a2)
  let lt (i1, a1) (i2, a2) = (Interval.lt i1 i2, AbsArray.join a1 a2)
  let ge (i1, a1) (i2, a2) = (Interval.ge i1 i2, AbsArray.join a1 a2)
  let gt (i1, a1) (i2, a2) = (Interval.gt i1 i2, AbsArray.join a1 a2)
  let eq (i1, a1) (i2, a2) = (Interval.eq i1 i2, AbsArray.join a1 a2)
  let not (i, a) = (Interval.not i, a)
  let band (i1, a1) (i2, a2) = (Interval.band i1 i2, AbsArray.join a1 a2)
  let bor (i1, a1) (i2, a2) = (Interval.bor i1 i2, AbsArray.join a1 a2)
  let to_string : t -> string 
  =fun (itv, arr) -> Printf.sprintf "(%s, %s)" (Interval.to_string itv) (AbsArray.to_string arr)
end 

module type AbsMem = sig
  type t = (AbsLoc.t, AbsVal.t) BatMap.t
  val empty : t
  val find : AbsLoc.t -> t -> AbsVal.t 
  val find_set : AbsLoc.t BatSet.t -> t -> AbsVal.t 
  val add : AbsLoc.t -> AbsVal.t -> t -> t
  val add_set : AbsLoc.t BatSet.t -> AbsVal.t -> t -> t 
  val join : t -> t -> t 
  val widen : t -> t -> t 
  val narrow : t -> t -> t
  val order : t -> t -> bool 
  val print : t -> unit 
end

module AbsMem : AbsMem = struct
  type t = (AbsLoc.t, AbsVal.t) BatMap.t
  let empty = BatMap.empty
  let default_val : AbsVal.t = (Interval.top, AbsArray.bot)
  let find x m = try BatMap.find x m with _ -> default_val
  let find_set xs m = BatSet.fold (fun x -> AbsVal.join (find x m)) xs AbsVal.bot 
  let strong_update loc v m = BatMap.add loc v m 
  let weak_update loc v m = BatMap.add loc (AbsVal.join (find loc m) v) m
  let add loc v m = 
    match loc with 
    | AbsLoc.Var _ -> strong_update loc v m 
    | AbsLoc.Allocsite _ -> weak_update loc v m 
  let add_set locs v m = 
    if BatSet.cardinal locs = 1 then add (BatSet.choose locs) v m 
    else BatSet.fold (fun loc -> weak_update loc v) locs m 
  let join m1 m2 = BatMap.foldi (fun x v m' -> add x (AbsVal.join v (find x m')) m') m1 m2
  let widen m1 m2 = BatMap.foldi (fun x v m' -> add x (AbsVal.widen v (find x m')) m') m1 m2
  let narrow m1 m2 = BatMap.foldi (fun x v m' -> add x (AbsVal.narrow v (find x m')) m') m1 m2
  let order m1 m2 = BatMap.for_all (fun x v -> AbsVal.order v (find x m2)) m1
  let print m = 
    BatMap.iter (fun x v -> 
      prerr_endline (AbsLoc.to_string x ^ " |-> " ^ AbsVal.to_string v)
    ) m 
end

module type Table = sig
  type t = AbsMem.t NodeMap.t
  val empty : t
  val init : Node.t list -> t 
  val find : Node.t -> t -> AbsMem.t 
  val add : Node.t -> AbsMem.t -> t -> t
  val print : t -> unit
end 

module Table : Table = struct 
  type t = AbsMem.t NodeMap.t
  let empty = NodeMap.empty 
  let add = NodeMap.add
  let init ns = List.fold_right (fun n -> add n AbsMem.empty) ns empty
  let find : Node.t -> t -> AbsMem.t 
  =fun n t -> try NodeMap.find n t with _ -> AbsMem.empty
  let print t = NodeMap.iter (fun n m -> 
    prerr_endline (string_of_int (Node.get_nodeid n)); 
    AbsMem.print m; 
    prerr_endline "") t  
end

let rec eval_a (e : S.exp) (mem : AbsMem.t) : Interval.t =
  match e with
  | S.NUM n      -> Interval.from_int n
  | S.LV (S.ID x) ->
      let (itv, _) = AbsMem.find (AbsLoc.Var x) mem in
      itv
  | S.LV (S.ARR (_,_)) ->
      Interval.top

  | S.ADD (e1,e2) -> Interval.add (eval_a e1 mem) (eval_a e2 mem)
  | S.SUB (e1,e2) -> Interval.sub (eval_a e1 mem) (eval_a e2 mem)
  | S.MUL (e1,e2) -> Interval.mul (eval_a e1 mem) (eval_a e2 mem)
  | S.DIV (e1,e2) -> Interval.div (eval_a e1 mem) (eval_a e2 mem)

  | S.MINUS e     -> Interval.sub Interval.zero (eval_a e mem)
  | S.NOT e       -> Interval.not (eval_a e mem)

  | S.LT (a,b)    -> Interval.lt (eval_a a mem) (eval_a b mem)
  | S.LE (a,b)    -> Interval.le (eval_a a mem) (eval_a b mem)
  | S.GT (a,b)    -> Interval.gt (eval_a a mem) (eval_a b mem)
  | S.GE (a,b)    -> Interval.ge (eval_a a mem) (eval_a b mem)
  | S.EQ (a,b)    -> Interval.eq (eval_a a mem) (eval_a b mem)

  | S.AND (a,b)   -> Interval.band (eval_a a mem) (eval_a b mem)
  | S.OR  (a,b)   -> Interval.bor  (eval_a a mem) (eval_a b mem)

let transfer (node : Node.t) (in_mem : AbsMem.t) : AbsMem.t =
  match Node.get_instr node with

  | I_alloc (x, n) ->
      let site = Node.get_nodeid node in
      let arr  = AbsArray.create site n in
      AbsMem.add (AbsLoc.Var x) (AbsVal.from_absarr arr) in_mem

  | I_assign (S.ID x, e) ->
      let rhs = eval_a e in_mem in
      AbsMem.add (AbsLoc.Var x) (AbsVal.from_itv rhs) in_mem

  | I_assign (S.ARR (x, _), _) ->
      let arr = AbsMem.find (AbsLoc.Var x) in_mem |> AbsVal.get_absarray in
      AbsMem.add (AbsLoc.Var x) (AbsVal.from_absarr arr) in_mem

  | I_assume cond ->
      let base = in_mem in
      let iv = eval_a cond base in
      if iv = Interval.zero then
        AbsMem.empty
      else
        let refine x ~tb ~fb =
          let (itv, arr) = AbsMem.find (AbsLoc.Var x) base in
          let m_t = AbsMem.add (AbsLoc.Var x) (Interval.meet itv tb, arr) base in
          let m_f = AbsMem.add (AbsLoc.Var x) (Interval.meet itv fb, arr) base in
          AbsMem.join m_t m_f
        in
        begin match cond with
        | S.LT (S.LV (S.ID x), S.NUM n) ->
            refine x
              ~tb:(Interval.Range (MinusInf, Int (n-1)))
              ~fb:(Interval.Range (Int (n-1), PlusInf))

        | S.LE (S.LV (S.ID x), S.NUM n) ->
            refine x
              ~tb:(Interval.Range (MinusInf, Int n))
              ~fb:(Interval.Range (Int n,       PlusInf))

        | S.GT (S.LV (S.ID x), S.NUM n) ->
            refine x
              ~tb:(Interval.Range (Int (n+1), PlusInf))
              ~fb:(Interval.Range (MinusInf,  Int n   ))

        | S.GE (S.LV (S.ID x), S.NUM n) ->
            refine x
              ~tb:(Interval.Range (Int n,      PlusInf))
              ~fb:(Interval.Range (MinusInf,  Int n   ))

        | S.EQ (S.LV (S.ID x), S.NUM n) ->
            let tb = Interval.Range (Int n, Int n) in
            let fb_lo = Interval.Range (MinusInf, Int (n-1)) in
            let fb_hi = Interval.Range (Int (n+1), PlusInf) in
            refine x ~tb ~fb:(Interval.join fb_lo fb_hi)

        | _ ->
            base
        end

  | I_skip
  | I_read  _ 
  | I_print _ ->
      in_mem


let fixpoint (cfg : Cfg.t) : Table.t =
  let nodes = Cfg.nodesof cfg in
  let tbl0  = Table.init nodes in
  let work  = Queue.create () in
  List.iter (fun n -> Queue.add n work) nodes;

  let rec loop tbl =
    if Queue.is_empty work then tbl else
    let n = Queue.take work in
    let old = Table.find n tbl in

    let pred_join =
      NodeSet.fold
        (fun p acc -> AbsMem.join acc (Table.find p tbl))
        (Cfg.preds n cfg)
        AbsMem.empty
    in
    let in_mem =
      if Cfg.is_loophead n cfg
      then AbsMem.widen old pred_join
      else AbsMem.join old pred_join
    in

    let stepped =
      match Node.get_instr n, Cfg.is_loophead n cfg with

      | I_assume cond, true ->
        begin match cond with
        | S.LT (S.LV (S.ID x), S.NUM k) ->
            let (itv, arr) = AbsMem.find (AbsLoc.Var x) in_mem in
            let bound      = Interval.Range (Interval.Int 1, Interval.Int (k-1)) in
            AbsMem.add (AbsLoc.Var x) (Interval.meet itv bound, arr) in_mem

        | S.LE (S.LV (S.ID x), S.NUM k) ->
            let (itv, arr) = AbsMem.find (AbsLoc.Var x) in_mem in
            let bound      = Interval.Range (Interval.Int 0, Interval.Int k) in
            AbsMem.add (AbsLoc.Var x) (Interval.meet itv bound, arr) in_mem

        | S.GT (S.LV (S.ID x), S.NUM k) ->
            let (itv, arr) = AbsMem.find (AbsLoc.Var x) in_mem in
            let bound      = Interval.Range (Interval.Int (k+1), Interval.PlusInf) in
            AbsMem.add (AbsLoc.Var x) (Interval.meet itv bound, arr) in_mem

        | S.GE (S.LV (S.ID x), S.NUM k) ->
            let (itv, arr) = AbsMem.find (AbsLoc.Var x) in_mem in
            let bound      = Interval.Range (Interval.Int k, Interval.PlusInf) in
            AbsMem.add (AbsLoc.Var x) (Interval.meet itv bound, arr) in_mem

        | _ ->
            in_mem
        end

      | _, _ ->
        transfer n in_mem
    in

    if not (AbsMem.order stepped old) then begin
      let tbl' = Table.add n stepped tbl in
      NodeSet.iter (fun s -> Queue.add s work) (Cfg.succs n cfg);
      loop tbl'
    end else
      loop tbl
  in
  loop tbl0

let rec has_type_error_exp (e : S.exp) (mem : AbsMem.t) : bool =
  match e with
  | S.NUM _ -> false
  | S.LV (S.ID x) ->
      let (_, arr) = AbsMem.find (AbsLoc.Var x) mem in
      not (BatSet.is_empty (AbsArray.get_allocsites arr))
  | S.LV (S.ARR (_, idx)) ->
      has_type_error_exp idx mem
  | S.ADD(a,b)|S.SUB(a,b)|S.MUL(a,b)|S.DIV(a,b)
  | S.LT(a,b)|S.LE(a,b)|S.GT(a,b)|S.GE(a,b)
  | S.EQ(a,b)|S.AND(a,b)|S.OR(a,b) ->
      has_type_error_exp a mem || has_type_error_exp b mem
  | S.MINUS a | S.NOT a ->
      has_type_error_exp a mem 

let inspect (cfg : Cfg.t) (tbl : Table.t) : bool =
  Cfg.nodesof cfg
  |> List.for_all (fun n ->
       let mem = Table.find n tbl in

       if mem = AbsMem.empty then true else

       match Node.get_instr n with

       | I_read x ->
         let (_, arr) = AbsMem.find (AbsLoc.Var x) mem in
         BatSet.is_empty (AbsArray.get_allocsites arr)

       | I_print e ->
         not (has_type_error_exp e mem)
         &&
         let rec scan e =
           match e with
           | S.DIV (_, d) ->
             let dv = eval_a d mem in
             let dl = (match dv with Interval.Range(l,_) -> l | _ -> Interval.MinusInf) in
             let du = (match dv with Interval.Range(_,u) -> u | _ -> Interval.PlusInf) in
             not (
               Interval.cmp_int dl (Interval.Int 0) <= 0
               && Interval.cmp_int (Interval.Int 0) du <= 0
             )
           | S.ADD(a,b)|S.SUB(a,b)|S.MUL(a,b)
           | S.LT(a,b)|S.LE(a,b)|S.GT(a,b)|S.GE(a,b)
           | S.EQ(a,b)|S.AND(a,b)|S.OR(a,b) ->
             scan a && scan b
           | S.MINUS a|S.NOT a ->
             scan a
           | _ -> true
         in
         scan e

       | I_assign (S.ID x, rhs) ->
         let (_, arrL) = AbsMem.find (AbsLoc.Var x) mem in
         if not (BatSet.is_empty (AbsArray.get_allocsites arrL)) then
           false
         else if has_type_error_exp rhs mem then
           false
         else
           let rec scan e =
             match e with
             | S.DIV (_, d) ->
               let dv = eval_a d mem in
               let dl = (match dv with Interval.Range(l,_) -> l | _ -> Interval.MinusInf) in
               let du = (match dv with Interval.Range(_,u) -> u | _ -> Interval.PlusInf) in
               not (
                 Interval.cmp_int dl (Interval.Int 0) <= 0
                 && Interval.cmp_int (Interval.Int 0) du <= 0
               )
             | S.ADD(a,b)|S.SUB(a,b)|S.MUL(a,b)
             | S.LT(a,b)|S.LE(a,b)|S.GT(a,b)|S.GE(a,b)
             | S.EQ(a,b)|S.AND(a,b)|S.OR(a,b) ->
               scan a && scan b
             | S.MINUS a|S.NOT a ->
               scan a
             | _ -> true
           in
           scan rhs

       | I_assign (S.ARR (x, idx), _) ->
         let (_, arrL) = AbsMem.find (AbsLoc.Var x) mem in
         if BatSet.is_empty (AbsArray.get_allocsites arrL) then
           false
         else if has_type_error_exp idx mem then
           false
         else
           let idx_iv = eval_a idx mem in
           let lb = (match idx_iv with Interval.Range(l,_) -> l | _ -> Interval.MinusInf) in
           let ub = (match idx_iv with Interval.Range(_,u) -> u | _ -> Interval.PlusInf) in
           let (_, size_iv) =
             AbsVal.get_absarray (AbsMem.find (AbsLoc.Var x) mem)
           in
           let max_idx =
             match size_iv with
             | Interval.Range (_, Interval.Int s) -> Interval.Int (s - 1)
             | _ -> Interval.PlusInf
           in
           not (
             Interval.cmp_int lb (Interval.Int 0) < 0
             || Interval.cmp_int ub max_idx       > 0
           )

       | I_assume cond ->
         not (has_type_error_exp cond mem)

       | I_skip
       | I_alloc _ ->
         true
     )

let analyze : Cfg.t -> bool 
=fun cfg -> 
  cfg 
  |> fixpoint  
  |> inspect cfg 