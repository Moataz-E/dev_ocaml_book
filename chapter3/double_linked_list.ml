(*
Define a parameterized type for doubly linked lists, using at least one record
with mutable fields.
*)
type 'a node = {
  value: 'a;
  mutable next_node: 'a link;
  mutable prev_node: 'a link;
} and type 'a link =
  | Empty
  | Node of 'a node
;;

exception Empty_list of string;;

(* assume we get the starting node as input dlist *)
let add e dlist =
  match dlist with
  | Empty -> Node {value = e; prev_node = Empty; next_node = Empty}
  | Node n as nd ->
    let new_node = {value = e; prev_node = n.prev; next_node = nd} in
    let new_link = Node new_node in
    n.prev <- new_link;
    begin
      match n.prev with
      | Empty -> ()
      | Node p -> p.next <- new_link;
    end
    new_link
;;

(* perform pointer operations for removing one node *)
let remove_node n =
  match n with
  | Empty -> raise (Empty_list "can't remove from an empty list!")
  | Node n as l ->
      match (n.prev, n.next) with
        | Empty, Empty -> Empty
        | Node p as n1, Empty -> p.next <- Empty; n1
        | Empty, Node nx as n2 -> nx.prev <- Empty; n2
        | (Node p as n1), (Node nx as n2) ->
          p.next <- n2; nx.prev <- n1; n1
;;

(* remove all nodes with element e in double linked list *)
let rec remove e l =
  let rec remove_left e2 =
    match e2 with
    | Empty -> ()
    | Node n as l -> let p = n.prev in
                     if n.value = e then ignore (remove_node n);
                     else remove_left p
  and remove_right e3 =
    match e3 with
    | Empty -> ()
    | Node n as l -> let nx = n.next in
                     if nx.value = e then ignore (remove_node n);
                     else remove_right e3
  in match l with
     | Empty -> Empty
     | Node n as l -> if n.value = e then remove e (remove_node l)
                      else (remove_left n.prev; remove_right n.next; l)
;;
