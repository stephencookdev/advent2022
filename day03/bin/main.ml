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

let get_p letter =
  let offset = if letter = Char.lowercase_ascii letter then 96 else 38 in
  Char.code letter - offset

let rec split_string_parts str n =
  match str with
  | "" -> []
  | _ ->
      String.sub str 0 n
      :: split_string_parts (String.sub str n (String.length str - n)) n

let split_string str n = split_string_parts str (Int.div (String.length str) n)

let get_score dupe_set =
  List.fold_left ( + ) 0 (List.map get_p (CharSet.elements dupe_set))

let rec find_dupes sets =
  match sets with
  | x :: y :: xs ->
      let dupes = CharSet.inter x y in
      if List.length xs = 0 then dupes
      else CharSet.inter dupes (find_dupes (dupes :: xs))
  | _ -> CharSet.empty

let str_to_set str =
  String.fold_left (fun acc x -> CharSet.add x acc) CharSet.empty str

let rec solve_part_1 input =
  match input with
  | [] -> 0
  | x :: xs ->
      get_score (find_dupes (List.map str_to_set (split_string x 2)))
      + solve_part_1 xs

let rec solve_part_2 input =
  match input with
  | x :: y :: z :: xs ->
      get_score (find_dupes (List.map str_to_set [ x; y; z ])) + solve_part_2 xs
  | _ -> 0

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
