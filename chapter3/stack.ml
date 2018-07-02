(*
The data structure 'a stack will be implemented in the form of a record
containing an array of elements and the first free position in this array is
tracked using an index pointer.
*)
type 'a stack = {
  mutable ind: int;
  mutable els: 'a array;
  size: int
};;

let init_stack n = {ind = 0; els = [||]; size = n};;

exception Stack_empty of string ;;
exception Stack_full of string;;

let pop s =
  if (s.ind = 0) then raise (Stack_empty "can't pop from an empty stack!")
  else
    s.ind <- (s.ind - 1);
    s.els.(s.ind)
;;

let push s e =
  if (s.els = [||]) then
    begin
      s.els <- Array.make s.size e;
      s.ind <- 1
    end
  else if ((s.ind + 1) > s.size) then
    raise (Stack_full "can't push, stack full!")
  else
    begin
      s.els.(s.ind) <- e;
      s.ind <- (s.ind + 1)
    end
;;

let s1 = init_stack 5;;
push s1 4;;
push s1 3;;
push s1 7;;
assert ((s1.els = [| 4; 3; 7; 4; 4; |]));;

let popped1 = pop s1;;
assert (popped1 = 7);;
