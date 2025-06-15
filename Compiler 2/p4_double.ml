exception NotImplemented;;


let double : ('a -> 'a) -> 'a -> 'a
= fun f ->
  fun x -> f (f x)