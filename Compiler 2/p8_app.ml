exception NotImplemented;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->

  let rec mem x lst =
    match lst with
    | [] -> false
    | h :: t -> (h = x) || mem x t
  in

  let rec filter_new l1 l2 =
    match l1 with
    | [] -> []
    | h :: t ->
        if mem h l2 then filter_new t l2
        else h :: filter_new t l2
  in

  l2 @ filter_new l1 l2