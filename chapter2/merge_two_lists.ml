let rec merge_i (l1: int list) (l2: int list) =
  match (l1, l2) with
    | ([], _) -> l2
    | (_, []) -> l1
    | (h1::t1, h2::t2) -> if (h1 <= h2) then h1 :: (merge_i t1 (h2::t2))
                                        else h2 :: (merge_i (h1::t1) t2)
;;

assert ((merge_i [] []) = []);;
assert ((merge_i [3; 6;] []) = [3; 6;]);; 
assert ((merge_i [1; 5; 10;] [2; 4;]) = [1; 2; 4; 5; 10;]);;


