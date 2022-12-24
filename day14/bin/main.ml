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

let rec parse_input input =
  match input with
  | [] -> []
  | raw_rock :: input_rest ->
      let raw_coordinates = Str.split (Str.regexp {| -> |}) raw_rock in
      List.map
        (fun raw_c ->
          let c = String.split_on_char ',' raw_c in
          { x = int_of_string (List.nth c 0); y = int_of_string (List.nth c 1) })
        raw_coordinates
      :: parse_input input_rest

(* let string_of_grid grid =
   let map_string = ref "" in
   for y = 0 to Array.length grid.(0) - 1 do
     for x = 0 to Array.length grid - 1 do
       map_string := !map_string ^ String.make 1 grid.(x).(y)
     done;
     map_string := !map_string ^ "\n"
   done;
   !map_string *)

let count_at_rest_sand grid =
  Array.fold_left
    (fun acc column ->
      acc
      + Array.fold_left
          (fun acc cell -> acc + if cell = 'o' then 1 else 0)
          0 column)
    0 grid

let get_sand_cursor grid sand =
  List.init 3 (fun i ->
      try grid.(sand.x - 1 + i).(sand.y + 1) with Invalid_argument _ -> '.')

let rec recurse_ticks grid death_floor_y falling_sand falling_sand_source =
  if death_floor_y > falling_sand.y then
    let falling_cursor = get_sand_cursor grid falling_sand in
    match falling_cursor with
    | [ _; '.'; _ ] ->
        recurse_ticks grid death_floor_y
          { x = falling_sand.x; y = falling_sand.y + 1 }
          falling_sand_source
    | [ '.'; _; _ ] ->
        recurse_ticks grid death_floor_y
          { x = falling_sand.x - 1; y = falling_sand.y + 1 }
          falling_sand_source
    | [ _; _; '.' ] ->
        recurse_ticks grid death_floor_y
          { x = falling_sand.x + 1; y = falling_sand.y + 1 }
          falling_sand_source
    | [ _; _; _ ] ->
        if grid.(falling_sand.x).(falling_sand.y) != 'o' then (
          grid.(falling_sand.x).(falling_sand.y) <- 'o';
          recurse_ticks grid death_floor_y falling_sand_source
            falling_sand_source)
    | _ -> failwith "Impossible grid state"

let rec set_single_rock rock grid offset =
  match rock with
  | from_coord :: to_coord :: remaining_coordinates ->
      let from_x = min from_coord.x to_coord.x in
      let from_y = min from_coord.y to_coord.y in
      let to_x = max from_coord.x to_coord.x in
      let to_y = max from_coord.y to_coord.y in
      for x = from_x - offset.x to to_x - offset.x do
        for y = from_y - offset.y to to_y - offset.y do
          grid.(x).(y) <- '#'
        done
      done;
      set_single_rock (to_coord :: remaining_coordinates) grid offset
  | _ -> ()

let rec set_rocks rocks grid offset =
  match rocks with
  | [] -> ()
  | r :: rs ->
      set_single_rock r grid offset;
      set_rocks rs grid offset

let create_rock_grid min max rocks =
  let grid = Array.make_matrix (max.x + 1 - min.x) (max.y + 1 - min.y) '.' in
  set_rocks rocks grid min;
  grid

let get_min_max_for_single_rock rock =
  let min_x = List.fold_left (fun acc c -> min acc c.x) max_int rock in
  let max_x = List.fold_left (fun acc c -> max acc c.x) min_int rock in
  let max_y = List.fold_left (fun acc c -> max acc c.y) min_int rock in
  (* +/- a window to lazily account for infinite floor *)
  ({ x = min_x - 10000; y = 0 }, { x = max_x + 10000; y = max_y + 2 })

let rec get_min_max_coords rocks =
  match rocks with
  | [] -> failwith "No min/max of an empty list of rocks"
  | r :: [] -> get_min_max_for_single_rock r
  | r :: rs ->
      let potential_min, potential_max = get_min_max_for_single_rock r in
      let r_potential_min, r_potential_max = get_min_max_coords rs in
      ( {
          x = min potential_min.x r_potential_min.x;
          y = min potential_min.y r_potential_min.y;
        },
        {
          x = max potential_max.x r_potential_max.x;
          y = max potential_max.y r_potential_max.y;
        } )

let solve_part input has_floor =
  let rocks = parse_input input in
  let min, max = get_min_max_coords rocks in
  let rock_grid = create_rock_grid min max rocks in
  if has_floor then
    set_single_rock [ { x = min.x; y = max.y }; max ] rock_grid min;
  let death_floor_y = max.y + 1 in
  (* print_endline (string_of_grid rock_grid); *)
  let falling_sand_source = { x = 500 - min.x; y = 0 } in
  recurse_ticks rock_grid death_floor_y falling_sand_source falling_sand_source;
  (* print_endline (string_of_grid rock_grid); *)
  let sand_count = count_at_rest_sand rock_grid in
  sand_count

let part1 input = string_of_int (solve_part input false)
let part2 input = string_of_int (solve_part input true);;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
