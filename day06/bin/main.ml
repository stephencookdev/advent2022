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

module CharSet = Set.Make (Char)

let rec is_unique = function
  | [] -> true
  | x :: xs -> (not (List.exists (( = ) x) xs)) && is_unique xs

let rec find_first_marker_index list marker_so_far prog marker_size =
  if List.length marker_so_far = marker_size && is_unique marker_so_far then
    prog
  else
    match (list, marker_so_far) with
    | [], _ -> -1
    | x :: xs, ys ->
        let new_marker_so_far =
          if List.length ys >= marker_size then List.append (List.tl ys) [ x ]
          else List.append ys [ x ]
        in
        find_first_marker_index xs new_marker_so_far (prog + 1) marker_size

let solve_part_1 input =
  let single_line_input = List.nth input 0 in
  let string_as_list =
    List.init (String.length single_line_input) (String.get single_line_input)
  in
  find_first_marker_index string_as_list [] 0 4

let solve_part_2 input =
  let single_line_input = List.nth input 0 in
  let string_as_list =
    List.init (String.length single_line_input) (String.get single_line_input)
  in
  find_first_marker_index string_as_list [] 0 14

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
