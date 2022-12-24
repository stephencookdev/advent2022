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

let parse_input_row row =
  List.map (List.map int_of_string)
    (List.map (String.split_on_char '-') (String.split_on_char ',' row))

let check_for_wasted_clean row =
  match row with
  | [ [ a1; a2 ]; [ b1; b2 ] ] ->
      (b1 <= a1 && b2 >= a2) || (a1 <= b1 && a2 >= b2)
  | _ -> failwith "Impossible"

let check_for_partially_wasted_clean row =
  match row with
  | [ [ a1; a2 ]; [ b1; b2 ] ] ->
      (b1 <= a1 && b2 >= a1) || (a1 <= b1 && a2 >= b1)
  | _ -> failwith "Impossible"

let solve_part_1 input =
  let rows = List.map parse_input_row input in
  let wasted_rows = List.filter check_for_wasted_clean rows in
  List.length wasted_rows

let solve_part_2 input =
  let rows = List.map parse_input_row input in
  let wasted_rows = List.filter check_for_partially_wasted_clean rows in
  List.length wasted_rows

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
