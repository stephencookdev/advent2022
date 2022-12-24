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

type 'a rec_list = Atom of 'a | RecList of 'a rec_list list

let add_to_rec_list (x : 'a rec_list) (xs : 'a rec_list) =
  match xs with
  | Atom _ -> failwith "Cannot add to an Atom tail"
  | RecList xs -> RecList (x :: xs)

let rec split_to_last list =
  match list with
  | [] -> failwith "No last exists"
  | x :: [] -> ([], x)
  | x :: xs ->
      let h, t = split_to_last xs in
      (x :: h, t)

let rec parse_block_raw block_raw input =
  match input with
  | [] -> block_raw
  | x :: xs ->
      if x = "" then parse_block_raw (List.append block_raw [ [] ]) xs
      else
        let h, t = split_to_last block_raw in
        parse_block_raw (List.append h [ List.append t [ x ] ]) xs

let sub_to_end s n = String.sub s n (String.length s - n)

let rec parse_block (block : string) : int rec_list * int =
  let start_char = String.get block 0 in
  if start_char = ',' then
    let sub_parsed, offset = parse_block (sub_to_end block 1) in
    (sub_parsed, offset + 1)
  else if start_char = ']' then (RecList [], 1)
  else if start_char = '[' then
    let sub_parsed, offset = parse_block (sub_to_end block 1) in
    let remaining_parsed, final_offset =
      parse_block (sub_to_end block (1 + offset))
    in
    (add_to_rec_list sub_parsed remaining_parsed, 1 + offset + final_offset)
  else
    let end_of_int_i = Str.search_forward (Str.regexp {|[^0-9]|}) block 0 in
    let n = int_of_string (String.sub block 0 end_of_int_i) in
    let remaining_parsed, offset =
      parse_block (sub_to_end block end_of_int_i)
    in
    (add_to_rec_list (Atom n) remaining_parsed, offset + end_of_int_i)

let parse_input input =
  let blocks = parse_block_raw [ [] ] input in
  List.map
    (fun raw_block ->
      let l = List.nth raw_block 0 in
      let r = List.nth raw_block 1 in
      let parsed_l, _ = parse_block (sub_to_end l 1) in
      let parsed_r, _ = parse_block (sub_to_end r 1) in
      (parsed_l, parsed_r))
    blocks

let rec compare_block x y =
  match x with
  | Atom x -> (
      match y with
      | Atom y -> x - y
      | RecList _ -> compare_block (RecList [ Atom x ]) y)
  | RecList xs -> (
      match y with
      | Atom y -> compare_block x (RecList [ Atom y ])
      | RecList ys -> (
          match (xs, ys) with
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | x :: xs, y :: ys ->
              let result = compare_block x y in
              if result = 0 then compare_block (RecList xs) (RecList ys)
              else result))

let is_decoder block =
  match block with
  | RecList [ RecList [ Atom n ] ] -> n == 2 || n == 6
  | _ -> false

let solve_part_1 input =
  let blocks = parse_input input in
  let block_positions = List.mapi (fun i _ -> i + 1) blocks in
  let correct_block_positions =
    List.filteri
      (fun i _ ->
        let l, r = List.nth blocks i in
        compare_block l r < 0)
      block_positions
  in
  List.fold_left ( + ) 0 correct_block_positions

let solve_part_2 input =
  let blocks = parse_input (List.append input [ ""; "[[2]]"; "[[6]]" ]) in
  let flat_blocks = List.flatten (List.map (fun (l, r) -> l :: [ r ]) blocks) in
  let sorted_flat_blocks = List.sort compare_block flat_blocks in
  let decoder_maps =
    List.mapi (fun i b -> if is_decoder b then i + 1 else 1) sorted_flat_blocks
  in
  List.fold_left ( * ) 1 decoder_maps

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
