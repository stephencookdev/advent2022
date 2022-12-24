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

let rec parse_input input =
  match input with
  | [] -> []
  | x :: xs -> (
      match String.split_on_char ' ' x with
      | [] -> raise (Invalid_argument "Cannot have empty instruction")
      | y :: ys -> (y, List.map int_of_string ys) :: parse_input xs)

let apply_single_instruction instruction x cycle =
  match instruction with
  | "noop", _ -> (x, cycle + 1)
  | "addx", [ x_to_add ] -> (x + x_to_add, cycle + 2)
  | _ -> raise (Invalid_argument "Invalid instruction")

let find_signals x_at_all_cycles special =
  List.map (fun s -> s * List.nth x_at_all_cycles (s - 1)) special

let rec apply_instructions instructions x cycle =
  match instructions with
  | [] -> []
  | instruction :: rest_of_instructions ->
      let new_x, new_cycle = apply_single_instruction instruction x cycle in
      List.append
        (List.init (new_cycle - cycle) (fun _ -> x))
        (apply_instructions rest_of_instructions new_x new_cycle)

let rec take list n =
  match n with
  | 0 -> ([], list)
  | n -> (
      match list with
      | [] -> ([], [])
      | x :: xs ->
          let l, r = take xs (n - 1) in
          (x :: l, r))

let rec break_into_groups x_at_all_cycles group_size =
  match x_at_all_cycles with
  | [] -> []
  | x_at_all_cycles ->
      let group, remainder = take x_at_all_cycles group_size in
      group :: break_into_groups remainder group_size

let make_single_crt_row x_per_cycle =
  let char_list =
    List.mapi
      (fun i x -> if x <= i + 1 && i + 1 <= x + 2 then '|' else ' ')
      x_per_cycle
  in
  String.of_seq (List.to_seq char_list)

let rec make_crt_rows x_at_per_row_cycles =
  match x_at_per_row_cycles with
  | [] -> []
  | x :: xs -> make_single_crt_row x :: make_crt_rows xs

let solve_part_1 input special =
  let instructions = parse_input input in
  let x_at_all_cycles = apply_instructions instructions 1 1 in
  let signal_strengths = find_signals x_at_all_cycles special in
  List.fold_left ( + ) 0 signal_strengths

let solve_part_2 input crt_width =
  let instructions = parse_input input in
  let x_at_all_cycles = apply_instructions instructions 1 1 in
  let x_at_per_row_cycles = break_into_groups x_at_all_cycles crt_width in
  let crt_rows = make_crt_rows x_at_per_row_cycles in
  List.fold_left (fun a b -> a ^ "\n" ^ b) "" crt_rows

let part1 input =
  string_of_int (solve_part_1 input [ 20; 60; 100; 140; 180; 220 ])

let part2 input = solve_part_2 input 40;;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
