(*
Lexical trees (or tries) are used for the representation of dictionaries.
*)

(*
bool in a lex_node marks the end of a valid word in the dictionary
when the bool's value is true.
*)
type lex_node =
  | Letter of char * bool * lex_tree
  and lex_tree = lex_node list
;;

type word = string;;


(*
Write the function exists which tests whether a word belongs to a
dictionary of type lex_tree.
*)
let rec exists (w: word) (ltr: lex_tree) =
  let aux w2 i n =
    match ltr2 with
    | [] -> false
    | Letter (c, b, lt)::t when (c = w2.[i]) ->
       if n = 1 then b
       else exists (String.sub w2 (i+1) (n-1)) lt
    | Letter (c, b, lt)::t -> exists sw t
  in aux w 0 (String.length w)
;;

let lex_node1 = Letter ('w', false, lex_tree1)
and lex_tree1 = [lex_node1]

(* to initialize, start from the bottom to the top. *)
