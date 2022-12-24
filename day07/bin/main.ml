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

let rec find_sizes lines count_so_far =
  match lines with
  | [] -> ([], count_so_far, [])
  | x :: xs ->
      if x = "$ ls" || String.starts_with ~prefix:"dir " x then
        find_sizes xs count_so_far
      else if x = "$ cd .." then ([], count_so_far, xs)
      else if String.starts_with ~prefix:"$ cd " x then
        let dir_name = String.sub x 5 (String.length x - 5) in
        let sub_sizes, total_sub_size, sub_remainder = find_sizes xs 0 in
        let sub_result = (dir_name, total_sub_size) :: sub_sizes in
        let remaining_result, new_count_so_far, new_remainder =
          find_sizes sub_remainder count_so_far
        in
        ( List.append sub_result remaining_result,
          total_sub_size + new_count_so_far,
          new_remainder )
      else
        let n = int_of_string (List.nth (String.split_on_char ' ' x) 0) in
        find_sizes xs (n + count_so_far)

let solve_part_1 input =
  let dir_size_tuples, _, _ = find_sizes input 0 in
  let dir_sizes = List.map (fun (_, n) -> n) dir_size_tuples in
  let in_range_dir_sizes = List.filter (fun n -> n < 100000) dir_sizes in
  List.fold_right ( + ) in_range_dir_sizes 0

let solve_part_2 input =
  let dir_size_tuples, total_dir_size, _ = find_sizes input 0 in
  let dir_sizes = List.map (fun (_, n) -> n) dir_size_tuples in
  let big_enough_dir_sizes =
    List.filter
      (fun n -> 70000000 - 30000000 - total_dir_size + n >= 0)
      dir_sizes
  in
  List.nth (List.sort (fun a b -> a - b) big_enough_dir_sizes) 0

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
