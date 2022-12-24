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

let rec index_of_list f = function
  | [] -> failwith "Not found"
  | x :: xs -> if f x then 0 else 1 + index_of_list f xs

let index_of_arr f arr =
  let rec loop i =
    if i = Array.length arr then failwith "Not found"
    else if f arr.(i) then i
    else loop (i + 1)
  in
  loop 0

let rec take n = function
  | [] -> []
  | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs

let rec drop n = function
  | [] -> []
  | x :: xs -> if n = 0 then x :: xs else drop (n - 1) xs

let parse_map raw_map =
  let height = List.length raw_map in
  let width = List.fold_left max 0 (List.map String.length raw_map) in
  let map = Array.make_matrix height width ' ' in
  List.iteri
    (fun y line -> String.iteri (fun x c -> map.(y).(x) <- c) line)
    raw_map;
  map

(* parse a string of numbers and letters into an array of integers
   and characters
   e.g. `parse_rules "R10L9R20"` would return `[(None, Some 'R'); (Some 10, None); (None, Some 'L'); (Some 9, None); (None, Some "R"); (Some 20, None)` *)
let rec parse_rules raw_rules =
  match raw_rules with
  | "" -> []
  | _ ->
      let rule =
        if String.get raw_rules 0 = 'L' then (None, Some false)
        else if String.get raw_rules 0 = 'R' then (None, Some true)
        else
          let n_to_parse =
            Str.search_forward (Str.regexp {|\([^0-9]\|$\)|}) raw_rules 0
          in
          let num = int_of_string (String.sub raw_rules 0 n_to_parse) in
          (Some num, None)
      in
      let remaining_raw_rules =
        match rule with
        | None, _ -> String.sub raw_rules 1 (String.length raw_rules - 1)
        | _ -> Str.replace_first (Str.regexp "[0-9]+") "" raw_rules
      in
      rule :: parse_rules remaining_raw_rules

let rec step_in_dir point facing map =
  let y, x = point in
  let fy, fx = facing in
  let new_y, new_x = (y + fy, x + fx) in
  let height, width = (Array.length map, Array.length map.(0)) in
  let wrapped_new_y =
    if new_y < 0 then height + new_y
    else if new_y >= height then new_y - height
    else new_y
  in
  let wrapped_new_x =
    if new_x < 0 then width + new_x
    else if new_x >= width then new_x - width
    else new_x
  in
  if map.(wrapped_new_y).(wrapped_new_x) = '#' then point
  else if map.(wrapped_new_y).(wrapped_new_x) = ' ' then
    let new_test_point = (wrapped_new_y, wrapped_new_x) in
    let end_point = step_in_dir new_test_point facing map in
    if end_point = new_test_point then point else end_point
  else (wrapped_new_y, wrapped_new_x)

let rec apply_rules rules point facing map =
  match rules with
  | [] -> (point, facing)
  | (Some n, _) :: remaining_rules ->
      let new_point = if n = 0 then point else step_in_dir point facing map in
      let new_rules =
        if n = 0 then remaining_rules
        else (Some (n - 1), None) :: remaining_rules
      in
      apply_rules new_rules new_point facing map
  | (_, Some turn) :: remaining_rules ->
      let fy, fx = facing in
      let new_facing = if turn then (fx, -fy) else (-fx, fy) in
      apply_rules remaining_rules point new_facing map
  | _ -> failwith "Invalid rule"

let parse_input input =
  let i_of_blank = index_of_list (fun x -> x = "") input in
  let raw_map = take i_of_blank input in
  let raw_rules = List.hd (drop (i_of_blank + 1) input) in
  let map = parse_map raw_map in
  let rules = parse_rules raw_rules in
  let starting_point = (0, index_of_arr (fun x -> x = '.') map.(0)) in
  let starting_facing = (0, 1) in
  let (fin_y, fin_x), final_facing =
    apply_rules rules starting_point starting_facing map
  in
  let row, column = (fin_y + 1, fin_x + 1) in
  let facing_score =
    match final_facing with
    | 0, 1 -> 0
    | 1, 0 -> 1
    | 0, -1 -> 2
    | -1, 0 -> 3
    | _ -> failwith "Invalid facing"
  in
  (row * 1000) + (column * 4) + facing_score

let part1 input = parse_input input
let part2 _input = 1;;

print_endline "";
print_endline ("Part 1: " ^ Batteries.dump (part1 read_input));
print_endline ("Part 2: " ^ Batteries.dump (part2 read_input))
