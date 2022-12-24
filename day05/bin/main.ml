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

let add_stack_row list listOfLists =
  List.init (List.length list) (fun n ->
      let maybeNthListItem = List.nth list n in
      let nthList =
        match List.nth_opt listOfLists n with
        | None -> []
        | Some nthList -> nthList
      in
      match maybeNthListItem with
      | None -> nthList
      | Some nthListItem -> nthListItem :: nthList)

let parse_stack_row row =
  (* Each row should be 4n+1 characters, for n columns, since [x] is 3 characters, plus 1 for spacing *)
  let size_of_row = (String.length row + 1) / 4 in
  List.init size_of_row (fun n ->
      let column = String.get row ((n * 4) + 1) in
      if column = ' ' then None else Some (String.make 1 column))

let parse_stacks input =
  (* So apparently regex in Ocaml is pure trash *)
  let row_regex = Str.regexp {|.*\[[A-Z]\]|} in
  let filtered_input =
    List.filter (fun row -> Str.string_match row_regex row 0) input
  in
  List.fold_right add_stack_row (List.map parse_stack_row filtered_input) []

let parse_instruction_row row =
  let r = Str.regexp {|move \([0-9]+\) from \([0-9]+\) to \([0-9]+\)|} in
  ignore (Str.search_forward r row 0);
  let n = Str.matched_group 1 row in
  let before1Index = Str.matched_group 2 row in
  let after1Index = Str.matched_group 3 row in
  ( int_of_string n,
    int_of_string before1Index - 1,
    int_of_string after1Index - 1 )

let parse_instructions input =
  let row_regex = Str.regexp {|move|} in
  let filtered_input =
    List.filter (fun row -> Str.string_match row_regex row 0) input
  in
  List.map parse_instruction_row filtered_input

let rec split n list =
  match (n, list) with
  | 0, xs -> ([], xs)
  | _, [] -> ([], [])
  | n, x :: xs ->
      let a, b = split (n - 1) xs in
      (x :: a, b)

let apply_instruction_to_stack stacks (to_take, before, after) =
  let to_move, to_keep = split to_take (List.nth stacks before) in
  List.init (List.length stacks) (fun i ->
      if i = before then to_keep
      else if i = after then List.append (List.rev to_move) (List.nth stacks i)
      else List.nth stacks i)

let apply_instruction_9001_to_stack stacks (to_take, before, after) =
  let to_move, to_keep = split to_take (List.nth stacks before) in
  List.init (List.length stacks) (fun i ->
      if i = before then to_keep
      else if i = after then List.append to_move (List.nth stacks i)
      else List.nth stacks i)

let get_top_of_stacks stacks = String.concat "" (List.map List.hd stacks)

let solve_part_1 input =
  let stacks = parse_stacks input in
  let instructions = parse_instructions input in
  let final_stacks =
    List.fold_left apply_instruction_to_stack stacks instructions
  in
  get_top_of_stacks final_stacks

let solve_part_2 input =
  let stacks = parse_stacks input in
  let instructions = parse_instructions input in
  let final_stacks =
    List.fold_left apply_instruction_9001_to_stack stacks instructions
  in
  get_top_of_stacks final_stacks

let part1 input = solve_part_1 input
let part2 input = solve_part_2 input;;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
