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
type scored_coordinate = { score : int; coordinate : coordinate }

let coordinates_are_equal a b = a.x = b.x && a.y = b.y

(* let string_of_coordinate coordinate =
   "(" ^ string_of_int coordinate.x ^ "," ^ string_of_int coordinate.y ^ ")" *)

let string_of_map map visited e =
  let map_string = ref "" in
  for y = 0 to Array.length map.(0) - 1 do
    for x = 0 to Array.length map - 1 do
      let cur = { x; y } in
      map_string :=
        !map_string
        ^
        if visited.(cur.x).(cur.y) != -1 then "."
        else if coordinates_are_equal cur e then "E"
        else String.make 1 (char_of_int map.(x).(y))
    done;
    map_string := !map_string ^ "\n"
  done;
  !map_string

let parse_map input =
  let s = ref { x = -1; y = -1 } in
  let e = ref { x = -1; y = -1 } in
  let height = List.length input in
  let width = String.length (List.nth input 0) in
  let map = Array.make_matrix width height 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let c = String.get (List.nth input y) x in
      let root_c =
        if c = 'S' then (
          s := { x; y };
          'a')
        else if c = 'E' then (
          e := { x; y };
          'z')
        else c
      in
      ignore (map.(x).(y) <- int_of_char root_c)
    done
  done;
  let visited = Array.make_matrix width height (-1) in
  (!s, !e, map, visited)

let get_jumps visited point map =
  let new_jumps = ref [] in
  for x = -1 to 1 do
    for y = -1 to 1 do
      try
        let jumped_coordinate =
          { x = point.coordinate.x + x; y = point.coordinate.y + y }
        in
        let cur_asc = map.(point.coordinate.x).(point.coordinate.y) in
        let jumped_asc = map.(jumped_coordinate.x).(jumped_coordinate.y) in
        let new_score = point.score + 1 in
        if
          (x != 0 || y != 0)
          && (x = 0 || y = 0)
          && (cur_asc < jumped_asc || cur_asc - jumped_asc <= 1)
          && (visited.(jumped_coordinate.x).(jumped_coordinate.y) = -1
             || visited.(jumped_coordinate.x).(jumped_coordinate.y) > new_score
             )
        then
          new_jumps :=
            { coordinate = jumped_coordinate; score = new_score } :: !new_jumps
      with Invalid_argument _ -> ()
    done
  done;
  visited.(point.coordinate.x).(point.coordinate.y) <- point.score;
  !new_jumps

let get_best_jump jumps =
  let sorted_list = List.sort (fun a b -> a.score - b.score) jumps in
  (List.hd sorted_list, List.tl sorted_list)

let remove_all x list =
  List.filter
    (fun y -> not (coordinates_are_equal x.coordinate y.coordinate))
    list

let rec unique_only list =
  match list with [] -> [] | x :: xs -> x :: unique_only (remove_all x xs)

let dedupe jumps =
  let sorted_list = List.sort (fun a b -> a.score - b.score) jumps in
  unique_only sorted_list

let rec make_best_jumps visited possible_jumps e map =
  let best_jump, remaining_possible_jumps = get_best_jump possible_jumps in
  print_endline (string_of_map map visited e);
  if best_jump.coordinate.x = e.x && best_jump.coordinate.y = e.y then
    best_jump.score
  else
    let new_jumps = get_jumps visited best_jump map in
    let all_new_jumps =
      dedupe (List.append remaining_possible_jumps new_jumps)
    in
    if List.length all_new_jumps = 0 then -1
    else make_best_jumps visited all_new_jumps e map

let solve_part_1 input =
  let s, e, map, visited = parse_map input in
  let starting_jumps = get_jumps visited { score = 0; coordinate = e } map in
  make_best_jumps visited starting_jumps s map

let solve_part_2 input =
  let _, e, map, visited = parse_map input in
  let starting_jumps = get_jumps visited { score = 0; coordinate = e } map in
  (* we only want the visited map, don't care about actual jumps *)
  ignore (make_best_jumps visited starting_jumps { x = -1; y = -1 } map);
  let all_a_scores = ref [] in
  for x = 0 to Array.length visited - 1 do
    for y = 0 to Array.length visited.(0) - 1 do
      if map.(x).(y) = int_of_char 'a' && visited.(x).(y) != -1 then
        all_a_scores := visited.(x).(y) :: !all_a_scores
    done
  done;
  List.hd (List.sort (fun a b -> a - b) !all_a_scores)

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
