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

type cube_side = {
  c : char;
  up : cube_side option ref;
  down : cube_side option ref;
  left : cube_side option ref;
  right : cube_side option ref;
}

module CMap = Map.Make (Char)

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

let rec step_in_dir_no_cube point facing map =
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
    let end_point = step_in_dir_no_cube new_test_point facing map in
    if end_point = new_test_point then point else end_point
  else (wrapped_new_y, wrapped_new_x)

let rec rotate_to_cube_side cube_side dir depth_limit =
  let rec attempt_multiple_rotations cube_side rotations depth_limit =
    match rotations with
    | [] -> (cube_side, [])
    | rotation :: rotations -> (
        try
          List.fold_left
            (fun (side, r_list) dir ->
              let new_side, new_r_list =
                rotate_to_cube_side side dir depth_limit
              in
              (new_side, r_list @ new_r_list))
            (cube_side, []) rotation
        with _ -> attempt_multiple_rotations cube_side rotations depth_limit)
  in

  if depth_limit = 0 then failwith "Too deep"
  else
    match dir with
    | "up" -> (
        match !(cube_side.up) with
        | Some up -> (up, [ (-1, 0) ])
        | None ->
            attempt_multiple_rotations cube_side
              [
                [ "down"; "down"; "down" ];
                [ "down"; "right"; "right" ];
                [ "down"; "left"; "left" ];
              ]
              (depth_limit - 1))
    | "down" -> (
        match !(cube_side.down) with
        | Some down -> (down, [ (1, 0) ])
        | None ->
            attempt_multiple_rotations cube_side
              [
                [ "up"; "up"; "up" ];
                [ "up"; "right"; "right" ];
                [ "up"; "left"; "left" ];
              ]
              (depth_limit - 1))
    | "left" -> (
        match !(cube_side.left) with
        | Some left -> (left, [ (0, -1) ])
        | None ->
            attempt_multiple_rotations cube_side
              [
                [ "right"; "right"; "right" ];
                [ "right"; "up"; "up" ];
                [ "right"; "down"; "down" ];
              ]
              (depth_limit - 1))
    | "right" -> (
        match !(cube_side.right) with
        | Some right -> (right, [ (0, 1) ])
        | None ->
            attempt_multiple_rotations cube_side
              [
                [ "left"; "left"; "left" ];
                [ "left"; "up"; "up" ];
                [ "left"; "down"; "down" ];
              ]
              (depth_limit - 1))
    | _ -> failwith "Invalid direction"

let get_center_point c cube_map =
  let height, width = (Array.length cube_map, Array.length cube_map.(0)) in
  let block_size = abs (width - height) in
  let rec find_start_point y x =
    if x >= width then find_start_point (y + 1) 0
    else if cube_map.(y).(x) = c then (y, x)
    else find_start_point y (x + 1)
  in
  let y1, x1 = find_start_point 0 0 in
  (y1 + (block_size / 2), x1 + (block_size / 2))

let rotate_point (y, x) (cy, cx) rlist =
  let py, px = (y - cy, x - cx) in
  let fpy, fpx =
    List.fold_left
      (fun (cur_py, cur_px) r ->
        match r with
        | 1, 0 -> (cur_px, -cur_py)
        | -1, 0 -> (-cur_px, cur_py)
        | 0, 1 -> (cur_py, cur_px)
        | 0, -1 -> (-cur_py, -cur_px)
        | _ -> failwith "Invalid rotation")
      (py, px) rlist
  in
  (fpy + cy, fpx + cx)

let traverse_cube (y, x) (fy, fx) map cube_map cube_lookup =
  let cur_cube_side_c = cube_map.(y).(x) in
  let cur_cube_side = CMap.find cur_cube_side_c cube_lookup in
  let plain_new_y, plain_new_x = (y + fy, x + fx) in
  if map.(plain_new_y).(plain_new_x) = ' ' then
    let new_side_c, rotation =
      match (fy, fx) with
      | 1, 0 ->
          let new_side_c, initial_rotation =
            rotate_to_cube_side cur_cube_side "down" 3
          in
          (new_side_c, initial_rotation @ [ (-1, 0) ])
      | -1, 0 ->
          let new_side_c, initial_rotation =
            rotate_to_cube_side cur_cube_side "up" 3
          in
          (new_side_c, initial_rotation @ [ (1, 0) ])
      | 0, 1 ->
          let new_side_c, initial_rotation =
            rotate_to_cube_side cur_cube_side "right" 3
          in
          (new_side_c, initial_rotation @ [ (0, -1) ])
      | 0, -1 ->
          let new_side_c, initial_rotation =
            rotate_to_cube_side cur_cube_side "left" 3
          in
          (new_side_c, initial_rotation @ [ (0, 1) ])
      | _ -> failwith "Invalid facing"
    in
    let rotated_facing = rotate_point (fy, fx) (0, 0) rotation in
    let rotated_point =
      rotate_point (y, x) (get_center_point new_side_c.c cube_map) rotation
    in
    (rotated_point, rotated_facing)
  else ((plain_new_y, plain_new_x), (fy, fx))

let step_in_dir_cube point facing map cube_map cube_lookup =
  let (new_y, new_x), new_facing =
    traverse_cube point facing map cube_map cube_lookup
  in
  if map.(new_y).(new_x) = '#' then (point, facing)
  else ((new_y, new_x), new_facing)

let step_in_dir point facing map = function
  | None -> (step_in_dir_no_cube point facing map, facing)
  | Some (cube_map, cube_lookup) ->
      step_in_dir_cube point facing map cube_map cube_lookup

let rec apply_rules rules point facing map cube_lookup =
  match rules with
  | [] -> (point, facing)
  | (Some n, _) :: remaining_rules ->
      let new_point, new_facing =
        if n = 0 then (point, facing)
        else step_in_dir point facing map cube_lookup
      in
      let new_rules =
        if n = 0 then remaining_rules
        else (Some (n - 1), None) :: remaining_rules
      in
      apply_rules new_rules new_point new_facing map cube_lookup
  | (_, Some turn) :: remaining_rules ->
      let fy, fx = facing in
      let new_facing = if turn then (fx, -fy) else (-fx, fy) in
      apply_rules remaining_rules point new_facing map cube_lookup
  | _ -> failwith "Invalid rule"

let parse_input input =
  let i_of_blank = index_of_list (fun x -> x = "") input in
  let raw_map = take i_of_blank input in
  let raw_rules = List.hd (drop (i_of_blank + 1) input) in
  let map = parse_map raw_map in
  let rules = parse_rules raw_rules in
  (map, rules)

let get_password (y, x) facing =
  let row, column = (y + 1, x + 1) in
  let facing_score =
    match facing with
    | 0, 1 -> 0
    | 1, 0 -> 1
    | 0, -1 -> 2
    | -1, 0 -> 3
    | _ -> failwith "Invalid facing"
  in
  (row * 1000) + (column * 4) + facing_score

let find_side_for x sides = List.find (fun s -> s.c = x) sides

let parse_cube map =
  let height = Array.length map in
  let width = Array.length map.(0) in
  let cube_map_height = if height > width then 4 else 3 in
  let cube_map_width = if height > width then 3 else 4 in
  let human_friendly_char_map =
    [|
      [| 'a'; 'b'; 'c'; 'd' |];
      [| 'e'; 'f'; 'g'; 'h' |];
      [| 'i'; 'j'; 'k'; 'l' |];
      [| 'm'; 'n'; 'o'; 'p' |];
    |]
  in
  let cube_map =
    Array.init height (fun y ->
        Array.init width (fun x ->
            if map.(y).(x) = ' ' then ' '
            else
              human_friendly_char_map.(y / cube_map_height).(x / cube_map_width)))
  in
  let cube_lookup =
    List.init (cube_map_height * cube_map_width) (fun i ->
        let y = i / cube_map_width in
        let x = i mod cube_map_width in
        cube_map.(y * height / cube_map_height).(x * width / cube_map_width))
    |> List.filter (fun x -> x != ' ')
    |> List.fold_left
         (fun cube c ->
           CMap.add c
             {
               c;
               up = ref None;
               down = ref None;
               left = ref None;
               right = ref None;
             }
             cube)
         CMap.empty
  in
  for y = 0 to cube_map_height - 1 do
    for x = 0 to cube_map_width - 1 do
      let c = cube_map.(y).(x) in
      if c != ' ' then (
        let add_side dir x =
          if x != ' ' then
            let side = CMap.find c cube_lookup in
            let result = Some (CMap.find x cube_lookup) in
            match dir with
            | "up" -> side.up := result
            | "down" -> side.down := result
            | "left" -> side.left := result
            | "right" -> side.right := result
            | _ -> failwith "Invalid side"
        in
        if y > 0 then add_side "up" cube_map.(y - 1).(x);
        if y < cube_map_height - 1 then add_side "down" cube_map.(y + 1).(x);
        if x > 0 then add_side "left" cube_map.(y).(x - 1);
        if x < cube_map_width - 1 then add_side "right" cube_map.(y).(x + 1))
    done
  done;
  (cube_map, cube_lookup)

let part1 input =
  let map, rules = parse_input input in
  let starting_point = (0, index_of_arr (fun x -> x = '.') map.(0)) in
  let starting_facing = (0, 1) in
  let final_point, final_facing =
    apply_rules rules starting_point starting_facing map None
  in
  get_password final_point final_facing

let part2 input =
  let map, rules = parse_input input in
  let starting_point = (0, index_of_arr (fun x -> x = '.') map.(0)) in
  let starting_facing = (0, 1) in
  let cube_map, cube_lookup = parse_cube map in
  let final_point, final_facing =
    apply_rules rules starting_point starting_facing map
      (Some (cube_map, cube_lookup))
  in
  get_password final_point final_facing
;;

print_endline "";
print_endline ("Part 1: " ^ Batteries.dump (part1 read_input));
print_endline ("Part 2: " ^ Batteries.dump (part2 read_input))
