exception NotImplemented;;

let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f l1 l2 c ->
  match (l1, l2) with
  | ([], []) -> c
  | (x :: xs, y :: ys) ->
      reduce f xs ys (f x y c)
  | _ ->
      failwith "Lists must have the same length"