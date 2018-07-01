(* 
Write a function merge_i which takes as input two integer lists 
sorted in increasing order and returns a new sorted list 
containing the elements of the first two.
*)
let rec merge_i (l1: int list) (l2: int list) =
  match (l1, l2) with
    | (_, []) -> l1
    | ([], _) -> l2
    | (h1::t1, h2::t2) -> if (h1 <= h2) 
                          then h1 :: (merge_i t1 (h2::t2))
                          else h2 :: (merge_i (h1::t1) t2)
;;

assert ((merge_i [] []) = []);;
assert ((merge_i [3; 6;] []) = [3; 6;]);; 
assert ((merge_i [1; 5; 10;] [2; 4;]) = [1; 2; 4; 5; 10;]);;


(*
Write a general function merge which takes as argument a comparison 
function and two lists sorted in this order and returns the list 
merged in the same order. The comparison function will be of 
type 'a -> 'a -> bool. 
*)
let rec merge (f: 'a -> 'a -> bool) (l1: 'a list) (l2: 'a list) =
  match (l1, l2) with
    | (_, []) -> l1
    | ([], _) -> l2
    | (h1::t1, h2::t2) -> if (f h1 h2)
                          then h1 :: (merge f t1 (h2::t2))
                          else h2 :: (merge f (h1::t1) t2)
;;

assert ((merge (fun x y -> x <= y) [] []) = []);;
assert ((merge (fun x y -> x <= y) [1; 4;] [3;]) = [1; 3; 4;]);;
assert ((merge (fun x y -> x >= y) [1; 4;] [3;]) = [3; 1; 4;]);;


(*
Write a new list type in the form of a record containing three 
fields: the conventional list, an order function and a boolean 
indicating whether the list is in that order.
*)
type 'a list2 = {
  cl: 'a list;
  orf: 'a -> 'a -> bool;
  in_order: bool;
};;


(*
Write the function insert which adds an element to a list of 
this type.
*)
let insert (e: 'a) (ls: 'a list2) = 
  let rec insert_rec e ls2 =
    match ls2 with
      | [] -> [e]
      | h :: t -> if (ls.orf e h)
                  then e :: h :: t
                  else h :: (insert_rec e t)
  in if ls.in_order
     then {ls with cl = insert_rec e ls.cl}
     else {ls with cl = (e :: ls.cl)}
;;

let test_ls1 = {cl = []; orf = (<); in_order = true};;
let test_ls2 = {cl = [1; 3; 9;]; orf = (<); in_order = true};;
assert ((insert 1 test_ls1).cl = [1;]);;
assert ((insert 4 test_ls2).cl = [1; 3; 4; 9;]);;


(* 
Write a function sort which insertion sorts the elements of 
a list.

Explanation: simply we initialize an empty list2 type and then
keep inserting values from the actual list into it using a 
fold_right operation. The fold_right operation runs the insert
function on all elements of the list starting from the last
element.
*)
let sort (ls: 'a list2) =
  if ls.in_order
  then ls
  else List.fold_right insert ls.cl {cl = []; orf = ls.orf; in_order = true}
;;

let test_ls3 = {cl = [5; 3; 7;]; orf = (<); in_order = false};;
assert ((sort test_ls3).cl = [3; 5; 7;]);;

(*
Write a new function merge for these lists.
*)
let rec merge_list2 (ls1: 'a list2) (ls2: 'a list2) =
  if ls1.in_order then 
    if ls2.in_order then {cl = merge ls1.orf ls1.cl ls2.cl;
                          orf = ls1.orf; in_order = true}
    else List.fold_right insert ls2.cl ls1
  else
    if ls2.in_order then merge_list2 ls2 ls1
    else merge_list2 (sort ls1) ls2
;;

assert ((merge_list2 test_ls1 test_ls2).cl = [1; 3; 9;]);;
assert ((merge_list2 test_ls2 test_ls3).cl = [1; 3; 3; 5; 7; 9;]);;  
