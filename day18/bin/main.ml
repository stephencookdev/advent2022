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

module IMap = Map.Make (Int)

let set_xyz_cell grid x y z cell_value =
  IMap.update x
    (fun y_grid ->
      let base_y_grid = match y_grid with None -> IMap.empty | Some y -> y in
      Some
        (IMap.update y
           (fun z_grid ->
             let base_z_grid =
               match z_grid with None -> IMap.empty | Some z -> z
             in
             Some (IMap.update z (fun _ -> Some cell_value) base_z_grid))
           base_y_grid))
    grid

let get_xyz_cell grid x y z =
  try IMap.find z (IMap.find y (IMap.find x grid)) with Not_found -> '?'

let get_all_adjoining grid fn x y z =
  [
    fn grid (x - 1) y z;
    fn grid (x + 1) y z;
    fn grid x (y - 1) z;
    fn grid x (y + 1) z;
    fn grid x y (z - 1);
    fn grid x y (z + 1);
  ]

let rec parse_input input =
  match input with
  | [] -> []
  | x :: xs ->
      let xyz = List.map int_of_string (String.split_on_char ',' x) in
      (List.nth xyz 0, List.nth xyz 1, List.nth xyz 2) :: parse_input xs

let rec count_cube_sides cubes_to_add grid min_xyz max_xyz =
  match cubes_to_add with
  | [] -> (grid, min_xyz, max_xyz, 0)
  | (x, y, z) :: remaining_cubes_to_add ->
      let face_surrounds = get_all_adjoining grid get_xyz_cell x y z in
      let blocked_face_count =
        List.fold_left ( + ) 0
          (List.map (fun x -> if x = '#' then 1 else 0) face_surrounds)
      in
      let new_grid = set_xyz_cell grid x y z '#' in
      let final_grid, final_min_xyz, final_max_xyz, side_diffs =
        count_cube_sides remaining_cubes_to_add new_grid
          (match min_xyz with
          | minx, miny, minz -> (min x minx, min y miny, min z minz))
          (match max_xyz with
          | maxx, maxy, maxz -> (max x maxx, max y maxy, max z maxz))
      in
      ( final_grid,
        final_min_xyz,
        final_max_xyz,
        6 - (2 * blocked_face_count) + side_diffs )

let rec get_true_air_grid queue (minx, miny, minz) (maxx, maxy, maxz)
    grid_of_lava true_air_grid =
  match queue with
  | [] -> true_air_grid
  | (x, y, z) :: queue_remainder ->
      if
        get_xyz_cell true_air_grid x y z = '?'
        && x >= minx && x <= maxx && y >= miny && y <= maxy && z >= minz
        && z <= maxz
      then
        let new_true_air_grid, new_queue_items =
          if get_xyz_cell grid_of_lava x y z = '#' then
            (set_xyz_cell true_air_grid x y z '#', [])
          else
            ( set_xyz_cell true_air_grid x y z '.',
              [
                (x - 1, y, z);
                (x + 1, y, z);
                (x, y - 1, z);
                (x, y + 1, z);
                (x, y, z - 1);
                (x, y, z + 1);
              ] )
        in
        get_true_air_grid
          (List.append new_queue_items queue_remainder)
          (minx, miny, minz) (maxx, maxy, maxz) grid_of_lava new_true_air_grid
      else
        get_true_air_grid queue_remainder (minx, miny, minz) (maxx, maxy, maxz)
          grid_of_lava true_air_grid

let get_air_pockets (minx, miny, minz) (maxx, maxy, maxz) grid_of_lava
    true_air_grid =
  let pockets_ref = ref [] in
  for x = minx + 1 to maxx - 1 do
    for y = miny + 1 to maxy - 1 do
      for z = minz + 1 to maxz - 1 do
        if
          get_xyz_cell grid_of_lava x y z != '#'
          && get_xyz_cell true_air_grid x y z != '.'
        then pockets_ref := (x, y, z) :: !pockets_ref
      done
    done
  done;
  !pockets_ref

let part1 input =
  let _, _, _, result =
    count_cube_sides (parse_input input) IMap.empty (0, 0, 0) (0, 0, 0)
  in
  string_of_int result

let part2 input =
  let grid_of_lava, min_xyz, max_xyz, result =
    count_cube_sides (parse_input input) IMap.empty (0, 0, 0) (0, 0, 0)
  in
  let true_air_grid =
    get_true_air_grid
      [ (match min_xyz with x, y, z -> (x - 1, y - 1, z - 1)) ]
      (match min_xyz with x, y, z -> (x - 1, y - 1, z - 1))
      (match max_xyz with x, y, z -> (x + 1, y + 1, z + 1))
      grid_of_lava IMap.empty
  in
  let air_pockets_to_fill =
    get_air_pockets min_xyz max_xyz grid_of_lava true_air_grid
  in
  let _, _, _, final_result =
    count_cube_sides air_pockets_to_fill grid_of_lava min_xyz max_xyz
  in
  string_of_int (result + final_result)
;;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
