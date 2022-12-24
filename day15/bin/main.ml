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

type coordinate = { x : int; y : int }

let coordinates_are_equal a b = a.x = b.x && a.y = b.y

let remove_all p list =
  List.filter (fun x -> not (coordinates_are_equal p x)) list

let rec unique_only list =
  match list with [] -> [] | x :: xs -> x :: unique_only (remove_all x xs)

let parse_coordinate raw_coordinate =
  let raw_useful =
    Str.global_replace (Str.regexp {|[^0-9-,]|}) "" raw_coordinate
  in
  let raw_x_y = String.split_on_char ',' raw_useful in
  let raw_x, raw_y = (List.nth raw_x_y 0, List.nth raw_x_y 1) in
  { x = int_of_string raw_x; y = int_of_string raw_y }

let rec parse_input input =
  match input with
  | [] -> []
  | raw_line :: rest_of_lines ->
      let raw_l_r = Str.split (Str.regexp {| closest beacon is |}) raw_line in
      let raw_l, raw_r = (List.nth raw_l_r 0, List.nth raw_l_r 1) in
      let l = parse_coordinate raw_l in
      let r = parse_coordinate raw_r in
      (l, r) :: parse_input rest_of_lines

let get_manhattan_distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)

let get_manhattan_distance_diffs coordinate points =
  List.map
    (fun (sensor, beacon) ->
      get_manhattan_distance sensor beacon
      - get_manhattan_distance sensor coordinate)
    points

let rec get_in_scope_cells points { x; y } max_x =
  if x > max_x then 0
  else
    let manhattan_distances_diffs =
      get_manhattan_distance_diffs { x; y } points
    in
    let biggest_manhattan_diff =
      List.fold_left max min_int manhattan_distances_diffs
    in
    if biggest_manhattan_diff >= 0 then
      let in_scope_to_add = biggest_manhattan_diff + 1 in
      let capped_in_scope_to_add =
        if x + in_scope_to_add > max_x then max_x - x else in_scope_to_add
      in
      capped_in_scope_to_add
      + get_in_scope_cells points { x = x + in_scope_to_add; y } max_x
    else
      get_in_scope_cells points { x = x + abs biggest_manhattan_diff; y } max_x

let rec get_min_max_x points =
  match points with
  | [] -> (max_int, min_int)
  | (sensor, beacon) :: rest_points ->
      let rest_min_x, rest_max_x = get_min_max_x rest_points in
      let spread = get_manhattan_distance sensor beacon in
      (min (sensor.x - spread) rest_min_x, max (sensor.x + spread) rest_max_x)

let get_beacon_count points y =
  let unique_beacons =
    unique_only (List.map (fun (_, beacon) -> beacon) points)
  in
  List.fold_left (fun acc p -> acc + if p.y = y then 1 else 0) 0 unique_beacons

let binary_search min_x max_x test =
  let lower_x = ref min_x in
  let upper_x = ref max_x in
  while !upper_x != !lower_x do
    let diff = !upper_x - !lower_x in
    if diff <= 1 then
      if test (!lower_x, !lower_x + 1) then upper_x := !lower_x
      else lower_x := !upper_x
    else
      let new_boundary = !lower_x + (diff / 2) in
      if test (!lower_x, new_boundary) then upper_x := new_boundary
      else lower_x := new_boundary
  done;
  !upper_x

let solve_part_1 input y =
  let points = parse_input input in
  let min_x, max_x = get_min_max_x points in
  let in_scope_count = get_in_scope_cells points { x = min_x; y } max_x in
  let beacon_count = get_beacon_count points y in
  in_scope_count - beacon_count

let solve_part_2 input max_bound =
  let points = parse_input input in
  let start_x, end_x = (0, max_bound) in
  let y = ref 0 in
  let final_y = ref (-1) in
  while !y < max_bound && !final_y = -1 do
    let in_scope_count =
      get_in_scope_cells points { x = start_x; y = !y } end_x
    in
    let possible_points = max_bound - in_scope_count in
    if possible_points = 0 then y := !y + 1 else final_y := !y
  done;
  if !final_y = -1 then -1
  else
    let x =
      binary_search 0 max_bound (fun (x1, x2) ->
          let in_scope_count =
            get_in_scope_cells points { x = x1; y = !y } x2
          in
          in_scope_count - x2 + x1 != 0)
    in
    (x * 4000000) + !final_y

(* let part1 input = string_of_int (solve_part_1 input 10)
   let part2 input = string_of_int (solve_part_2 input 20);; *)

let part1 input = string_of_int (solve_part_1 input 2000000)
let part2 input = string_of_int (solve_part_2 input 4000000);;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
