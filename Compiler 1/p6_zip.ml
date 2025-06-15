exception NotImplemented;;

let rec zip : int list * int list -> int list
= fun (l1, l2) ->
  match (l1, l2) with
  | ([], []) -> []
  | ([], b) -> b          
  | (a, []) -> a          
  | (x :: xs, y :: ys) -> x :: y :: zip (xs, ys)