let read_input =
  let lines = ref [] in
  let chan = open_in "./bin/input.txt" in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* print_endline "";
   print_endline ("Part 1: " ^ Batteries.dump (part1 read_input));
   print_endline ("Part 2: " ^ Batteries.dump (part2 read_input)) *)
