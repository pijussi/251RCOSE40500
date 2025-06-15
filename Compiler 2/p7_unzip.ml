exception NotImplemented;;

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  match lst with
  | [] -> ([], [])
  | (x, y) :: rest ->
      let (xs, ys) = unzip rest in
      (x :: xs, y :: ys)