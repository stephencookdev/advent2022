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
  | x :: xs ->
      (String.get x 0, int_of_string (String.sub x 2 (String.length x - 2)))
      :: parse_input xs

let get_tail_relative_coordinates ((hx, hy), (tx, ty)) = (hx - tx, hy - ty)

let get_follow_tail_offset tail_relative_head =
  match tail_relative_head with
  | 0, 2 -> (0, 1)
  | 1, 2 -> (1, 1)
  | -1, 2 -> (-1, 1)
  | 2, 0 -> (1, 0)
  | 2, 1 -> (1, 1)
  | 2, -1 -> (1, -1)
  | 0, -2 -> (0, -1)
  | 1, -2 -> (1, -1)
  | -1, -2 -> (-1, -1)
  | -2, 0 -> (-1, 0)
  | -2, 1 -> (-1, 1)
  | -2, -1 -> (-1, -1)
  | 2, 2 -> (1, 1)
  | -2, 2 -> (-1, 1)
  | 2, -2 -> (1, -1)
  | -2, -2 -> (-1, -1)
  (* Any other case doesn't need a move *)
  | _ -> (0, 0)

let rec fix_tail head tail =
  match tail with
  | [] -> []
  | (tx, ty) :: rest_of_tail ->
      let tail_relative_coordinates =
        get_tail_relative_coordinates (head, (tx, ty))
      in
      let tox, toy = get_follow_tail_offset tail_relative_coordinates in
      let new_top_tail = (tx + tox, ty + toy) in
      new_top_tail :: fix_tail new_top_tail rest_of_tail

let apply_single_instruction rope dir =
  match rope with
  | [] -> raise (Invalid_argument "Cannot have empty rope")
  | (hx, hy) :: rope_tail ->
      let new_head =
        match dir with
        | 'R' -> (hx + 1, hy)
        | 'L' -> (hx - 1, hy)
        | 'U' -> (hx, hy + 1)
        | 'D' -> (hx, hy - 1)
        | _ -> raise (Invalid_argument "Not a valid instruction")
      in
      new_head :: fix_tail new_head rope_tail

let rec apply_instructions coordinates instructions =
  match instructions with
  | [] -> []
  | (_, 0) :: remaining_instructions ->
      apply_instructions coordinates remaining_instructions
  | (dir, n) :: remaining_instructions ->
      let new_coord = apply_single_instruction coordinates dir in
      new_coord
      :: apply_instructions new_coord ((dir, n - 1) :: remaining_instructions)

let rec get_tail coordinates =
  match coordinates with
  | [] -> raise (Invalid_argument "Cannot get tail of empty rope")
  | [ _; x ] -> x
  | _ :: xs -> get_tail xs

let rec get_tail_trail trail =
  match trail with
  | [] -> []
  | rope :: trail_remainder -> get_tail rope :: get_tail_trail trail_remainder

let remove_all (x1, y1) list =
  List.filter (fun (x2, y2) -> x1 != x2 || y1 != y2) list

let rec unique_only list =
  match list with [] -> [] | x :: xs -> x :: unique_only (remove_all x xs)

let solve_part input n =
  let instructions = parse_input input in
  let init_coordinates = List.init n (fun _ -> (0, 0)) in
  let trail = apply_instructions init_coordinates instructions in
  let tail_only_trail = get_tail_trail trail in
  let unique_tail = unique_only tail_only_trail in
  List.length unique_tail

let part1 input = string_of_int (solve_part input 2)
let part2 input = string_of_int (solve_part input 10);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
