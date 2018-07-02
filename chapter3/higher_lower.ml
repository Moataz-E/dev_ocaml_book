(*
Implementation of the Higher/Lower game where the program interacts with the
player until the player successfully guesses the number
*)

let rec hilo n =
  let () = print_string "Type a number: " in
    let i = read_int() in
      if (n = i) then
        let () = print_string "Correct!" in
        print_newline()
      else
        let () =
          if (i < n) then
            let () = print_string "Higher!" in
            print_newline()
          else
           let () = print_string "Lower!" in
           print_newline()
        in hilo n
;;

let () = print_string "Select a number to play the game with: " in
  let n = read_int() in
    hilo n;;
