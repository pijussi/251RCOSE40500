exception NotImplemented;;

exception NotImplemented;;

type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  
  let digit_to_int d =
    match d with
    | ZERO -> 0
    | ONE  -> 1
  in

  
  let bin_to_int (b : bin) : int =
    List.fold_left (fun acc d -> acc * 2 + digit_to_int d) 0 b
  in

  
  let int_to_bin (n : int) : bin =
    if n = 0 then [ZERO]
    else
      let rec loop x acc =
        if x = 0 then acc
        else
          let d = if x mod 2 = 0 then ZERO else ONE in
          loop (x / 2) (d :: acc)
      in
      loop n []
  in

  int_to_bin (bin_to_int b1 * bin_to_int b2)
