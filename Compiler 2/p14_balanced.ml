exception NotImplemented;;

type mobile = branch * branch
and branch =
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
and length = int
and weight = int

let rec total_weight (b : branch) : int =
  match b with
  | SimpleBranch (_, w) -> w
  | CompoundBranch (_, (lb, rb)) ->
      total_weight lb + total_weight rb

let rec balanced (m : mobile) : bool =
  let (left_b, right_b) = m in

  let left_len =
    match left_b with
    | SimpleBranch (l, _) -> l
    | CompoundBranch (l, _) -> l
  in
  let right_len =
    match right_b with
    | SimpleBranch (l, _) -> l
    | CompoundBranch (l, _) -> l
  in

  let left_w = total_weight left_b in
  let right_w = total_weight right_b in

  (left_len * left_w = right_len * right_w)
  && branch_balanced left_b
  && branch_balanced right_b

and branch_balanced (b : branch) : bool =
  match b with
  | SimpleBranch _ ->
      true
  | CompoundBranch (_, sub_mobile) ->
      balanced sub_mobile