exception NotImplemented;;

type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

  let calculator : exp -> int
  = fun e ->
  
    let rec eval e xval =
      match e with
      | X -> xval
      | INT n -> n
      | ADD (e1, e2) -> eval e1 xval + eval e2 xval
      | SUB (e1, e2) -> eval e1 xval - eval e2 xval
      | MUL (e1, e2) -> eval e1 xval * eval e2 xval
      | DIV (e1, e2) -> eval e1 xval / eval e2 xval
      | SIGMA (start_exp, end_exp, body_exp) ->
          let start_val = eval start_exp xval in
          let end_val   = eval end_exp xval in
          let rec loop i acc =
            if i > end_val then acc
            else loop (i + 1) (acc + eval body_exp i)
          in
          loop start_val 0
    in
  
    eval e 0