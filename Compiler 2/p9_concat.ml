exception NotImplemented;;

let rec concat : 'a list list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | x :: xs -> x @ concat xs