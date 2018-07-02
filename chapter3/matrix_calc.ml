(*
The monomorphic type mat is a record containing the dimensions and the elements
of the matrix.
*)
type mat = { n: int; m: int; content: float array array};;

exception Incompatible_dimensions of string;;

let create_mat n m = {n = n; m = m; content = Array.make_matrix n m 0.0 };;
let access_mat mtx i j = mtx.content.(i).(j);;
let modify_mat mtx i j v = mtx.content.(i).(j) <- v;;

(* Find the sum of two matrics *)
let add_mat m1 m2 =
  if not ((m1.n = m2.n) && (m1.m = m2.m)) then
    raise (Incompatible_dimensions "matrices need to have equal dimensions.")
  else
    let result = create_mat m1.n m1.m in
    for i = 0 to (m1.n - 1) do
      for j = 0 to (m1.m - 1) do
        modify_mat result i j (m1.content.(i).(j) +. m2.content.(i).(j))
      done
    done;
    result
;;

(* Find the product of two matrices *)
let prod_mat m1 m2 =
  if not (m1.m = m2.n) then
    raise (Incompatible_dimensions
      "number of columns of first matrix should equal rows of second.")
  else
    let result = create_mat m1.n m2.m in
    for i = 0 to (m1.n - 1) do
      for j = 0 to (m1.m - 1) do
        let acc = ref 0.0 in
        for k = 0 to (m2.n - 1) do
          acc := !acc +. (m1.content.(i).(k) *. m2.content.(k).(j))
        done;
        modify_mat result i j !acc
      done
    done;
    result
;;

(* Create a 2x2 matrix with values:
  1  7
  5  4
*)
let mat1 = create_mat 2 2;;
modify_mat mat1 0 0 1.;;
modify_mat mat1 0 1 7.;;
modify_mat mat1 1 0 5.;;
modify_mat mat1 1 1 4.;;

(* Create a second 2x2 matrix with values:
  3  1
  9  6
*)
let mat2 = create_mat 2 2;;
modify_mat mat2 0 0 3.;;
modify_mat mat2 0 1 1.;;
modify_mat mat2 1 0 9.;;
modify_mat mat2 1 1 6.;;

(* Create and test result matrix of adding the above two matrices up *)
let add_mat_res = create_mat 2 2;;
modify_mat add_mat_res 0 0 4.;;
modify_mat add_mat_res 0 1 8.;;
modify_mat add_mat_res 1 0 14.;;
modify_mat add_mat_res 1 1 10.;;
assert ((add_mat mat1 mat2).content = add_mat_res.content);;

(* Create and test result of multiplying the above two matrices *)
let prod_mat_res = create_mat 2 2;;
modify_mat prod_mat_res 0 0 66.;;
modify_mat prod_mat_res 0 1 43.;;
modify_mat prod_mat_res 1 0 51.;;
modify_mat prod_mat_res 1 1 29.;;
assert ((prod_mat mat1 mat2).content = prod_mat_res.content);;
