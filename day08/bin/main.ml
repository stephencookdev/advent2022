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

let count_visible grid =
  Array.fold_left
    (fun acc sub_arr_x ->
      Array.fold_left
        (fun acc_inner is_visible -> acc_inner + if is_visible then 1 else 0)
        acc sub_arr_x)
    0 grid

let determine_visibility grid lr rl ud du =
  let max_grid_offsets = 1 in
  Array.mapi
    (fun i row ->
      Array.mapi
        (fun j _ ->
          List.exists
            (fun n -> grid.(i).(j) > n)
            [
              lr.(i - 1 + max_grid_offsets).(j + max_grid_offsets);
              rl.(i + 1 + max_grid_offsets).(j + max_grid_offsets);
              ud.(i + max_grid_offsets).(j - 1 + max_grid_offsets);
              du.(i + max_grid_offsets).(j + 1 + max_grid_offsets);
            ])
        row)
    grid

let get_max grid x y =
  let base_matrix =
    Array.make_matrix (Array.length grid + 2) (Array.length grid.(0) + 2) (-1)
  in
  let x_from, x_to =
    if x = -1 then (Array.length grid - 1, -1)
    else if x = 0 then (0, Array.length grid - 1)
    else (0, Array.length grid)
  in
  let y_from, y_to =
    if y = -1 then (Array.length grid.(0) - 1, -1)
    else if y = 0 then (0, Array.length grid.(0) - 1)
    else (0, Array.length grid.(0))
  in
  let update_matrix i j =
    base_matrix.(i + 1).(j + 1) <-
      Int.max
        base_matrix.(i + 1 - x).(j + 1 - y)
        (try grid.(i).(j) with Invalid_argument _ -> 0)
  in
  let i = ref x_from in
  let j = ref y_from in
  while if x = -1 then !i >= x_to else !i <= x_to do
    j := y_from;
    while if y = -1 then !j >= y_to else !j <= y_to do
      update_matrix !i !j;
      j := !j + if y = -1 then -1 else 1
    done;
    i := !i + if x = -1 then -1 else 1
  done;
  base_matrix

let get_scenic_number grid x y =
  let base_matrix =
    Array.make_matrix (Array.length grid) (Array.length grid.(0)) (-1)
  in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      let count = ref 0 in
      let look_for_trees = ref true in
      while !look_for_trees do
        try
          if
            grid.(i - (x * (!count + 1))).(j - (y * (!count + 1)))
            >= grid.(i).(j)
          then look_for_trees := false;
          count := !count + 1
        with Invalid_argument _ -> look_for_trees := false
      done;
      base_matrix.(i).(j) <- !count
    done
  done;
  base_matrix

let parse_grid input =
  Array.init
    (String.length (List.nth input 0))
    (fun x ->
      Array.init (List.length input) (fun y ->
          int_of_string (String.make 1 (String.get (List.nth input y) x))))

let get_best_scenic_number lr rl ud du =
  let combined =
    Array.mapi
      (fun i _ ->
        Array.mapi
          (fun j _ -> lr.(i).(j) * rl.(i).(j) * ud.(i).(j) * du.(i).(j))
          lr.(i))
      lr
  in
  Array.fold_left
    (fun n row -> Int.max n (Array.fold_left Int.max 0 row))
    0 combined

let solve_part_1 input =
  let grid = parse_grid input in
  let max_lr = get_max grid 1 0 in
  let max_rl = get_max grid (-1) 0 in
  let max_ud = get_max grid 0 1 in
  let max_du = get_max grid 0 (-1) in
  let is_visible_grid = determine_visibility grid max_lr max_rl max_ud max_du in
  count_visible is_visible_grid

let solve_part_2 input =
  let grid = parse_grid input in
  let scenic_lr = get_scenic_number grid 1 0 in
  let scenic_rl = get_scenic_number grid (-1) 0 in
  let scenic_ud = get_scenic_number grid 0 1 in
  let scenic_du = get_scenic_number grid 0 (-1) in
  let best_scenic =
    get_best_scenic_number scenic_lr scenic_rl scenic_ud scenic_du
  in
  best_scenic

let part1 input = string_of_int (solve_part_1 input)
let part2 input = string_of_int (solve_part_2 input);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
