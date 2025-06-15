exception NotImplemented;;

type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

(* diff : aexp * string -> aexp *)
let rec diff (exp, x) =
  match exp with
  | Const _ ->
      Const 0

  | Var v ->
      if v = x then Const 1 else Const 0

  | Power (v, n) ->
      if v = x then
        Times [Const n; Power (v, n - 1)]
      else
        Const 0

  | Sum elist ->
      Sum (List.map (fun e -> diff (e, x)) elist)

  | Times elist ->
      diff_times elist x


and diff_times (factors : aexp list) (x : string) : aexp =
  match factors with
  | [] ->
      
      Const 0
  | [single] ->
      
      diff (single, x)
  | _ ->
      
      let n = List.length factors in

      
      let remove_i lst i =
        let rec aux idx acc = function
          | [] -> List.rev acc
          | h :: t ->
              if idx = i then List.rev_append acc t
              else aux (idx + 1) (h :: acc) t
        in
        aux 0 [] lst
      in

      
      let rec build_sum i =
        if i >= n then []
        else
          let d_ai = diff (List.nth factors i, x) in
          let others = remove_i factors i in
          
          let term = Times (d_ai :: others) in
          term :: build_sum (i + 1)
      in

      Sum (build_sum 0)