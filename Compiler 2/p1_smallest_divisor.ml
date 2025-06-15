exception NotImplemented;;

let smallest_divisor : int -> int =
  fun n ->
    let rec find d =
      if d * d > n then n
      else if n mod d = 0 then d
      else find (d + 1)
    in
    find 2