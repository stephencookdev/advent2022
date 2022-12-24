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

module SMap = Map.Make (String)

type coordinate = { x : int; y : int }

type rock = {
  coordinate : coordinate;
  height : int;
  id : int;
  grid : bool array array;
}

let parse_input input =
  match input with
  | [ s ] ->
      Array.init
        (String.length s * 2)
        (fun i -> if i mod 2 = 0 then String.get s (i / 2) else 'v')
  | _ -> failwith "Invalid input"

(* let string_of_grid grid =
   let map_string = ref "" in
   for y = Array.length grid - 1 downto 0 do
     for x = 0 to Array.length grid.(0) - 1 do
       map_string := !map_string ^ if grid.(y).(x) then "#" else "."
     done;
     map_string := !map_string ^ "\n"
   done;
   !map_string *)

let string_of_grid_optimised grid =
  Batteries.dump
    (Array.map
       (fun row ->
         string_of_int
           (Array.fold_left ( + ) 0
              (Array.mapi
                 (fun i cell -> Int.shift_left (if cell then 1 else 0) i)
                 row)))
       grid)

let local_grid_intersects grid1 grid2 =
  Array.exists2
    (fun row1 row2 ->
      Array.exists2 (fun cell1 cell2 -> cell1 && cell2) row1 row2)
    grid1 grid2

let local_grid grid point size =
  Array.init size.y (fun y ->
      Array.init size.x (fun x ->
          try grid.(y + point.y).(x + point.x) with Invalid_argument _ -> true))

let set_rock grid rock =
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j cell ->
          let x = j + rock.coordinate.x in
          let y = i + rock.coordinate.y in
          try grid.(y).(x) <- cell || grid.(y).(x)
          with Invalid_argument _ -> ignore "Do nothing")
        row)
    rock.grid

let gen_rocks_from_char_array arr =
  let length_y = Array.length arr in
  let length_x = Array.length arr.(0) in
  Array.init length_y (fun y ->
      Array.init length_x (fun x -> arr.(length_y - y - 1).(x) = '#'))

let gen_rock id coordinate =
  let possible_rocks =
    [
      {
        id = 0;
        coordinate;
        height = 1;
        grid =
          gen_rocks_from_char_array
            [|
              [| '.'; '.'; '.'; '.' |];
              [| '.'; '.'; '.'; '.' |];
              [| '.'; '.'; '.'; '.' |];
              [| '#'; '#'; '#'; '#' |];
            |];
      };
      {
        id = 1;
        coordinate;
        height = 3;
        grid =
          gen_rocks_from_char_array
            [|
              [| '.'; '.'; '.'; '.' |];
              [| '.'; '#'; '.'; '.' |];
              [| '#'; '#'; '#'; '.' |];
              [| '.'; '#'; '.'; '.' |];
            |];
      };
      {
        id = 2;
        coordinate;
        height = 3;
        grid =
          gen_rocks_from_char_array
            [|
              [| '.'; '.'; '.'; '.' |];
              [| '.'; '.'; '#'; '.' |];
              [| '.'; '.'; '#'; '.' |];
              [| '#'; '#'; '#'; '.' |];
            |];
      };
      {
        id = 3;
        coordinate;
        height = 4;
        grid =
          gen_rocks_from_char_array
            [|
              [| '#'; '.'; '.'; '.' |];
              [| '#'; '.'; '.'; '.' |];
              [| '#'; '.'; '.'; '.' |];
              [| '#'; '.'; '.'; '.' |];
            |];
      };
      {
        id = 4;
        coordinate;
        height = 2;
        grid =
          gen_rocks_from_char_array
            [|
              [| '.'; '.'; '.'; '.' |];
              [| '.'; '.'; '.'; '.' |];
              [| '#'; '#'; '.'; '.' |];
              [| '#'; '#'; '.'; '.' |];
            |];
      };
    ]
  in
  let capped_id = id mod List.length possible_rocks in
  List.find (fun rock -> rock.id = capped_id) possible_rocks

let rec find_height_of_all_covered grid y prog =
  if y < 0 then 0
  else
    let new_prog =
      Array.mapi (fun x prog_cell -> prog_cell || grid.(y).(x)) prog
    in
    if Array.for_all (fun x -> x) new_prog then 0
    else 1 + find_height_of_all_covered grid (y - 1) new_prog

let rec recurse_ticks loop_detector_cache grid rock tick wind_flow rocks_to_drop
    max_height_so_far =
  let capped_tick = tick mod Array.length wind_flow in
  let height_of_grid_to_copy =
    find_height_of_all_covered grid max_height_so_far
      (Array.make (Array.length grid.(0)) false)
  in
  let grid_to_copy =
    local_grid grid
      { x = 0; y = max_height_so_far - height_of_grid_to_copy }
      { x = Array.length grid.(0); y = height_of_grid_to_copy }
  in
  let cache_key =
    string_of_int capped_tick ^ "_" ^ string_of_int rock.id ^ "_"
    ^ string_of_grid_optimised grid_to_copy
  in
  if SMap.mem cache_key !loop_detector_cache then (
    let rocks_dropped, max_height_at_time =
      SMap.find cache_key !loop_detector_cache
    in
    print_endline "Loop detected!";
    let diff_in_rock_drops = rocks_dropped - rocks_to_drop in
    let times_to_loop = rocks_to_drop / diff_in_rock_drops in
    if times_to_loop > 0 then (
      let skip_x_rock_drops = diff_in_rock_drops * times_to_loop in
      let height_in_skip =
        times_to_loop * (max_height_so_far - max_height_at_time)
      in
      let new_rocks_to_drop = rocks_to_drop - skip_x_rock_drops in
      print_endline
        ("Between "
        ^ string_of_int rocks_dropped
        ^ " and "
        ^ string_of_int rocks_to_drop
        ^ ", skipping "
        ^ string_of_int diff_in_rock_drops
        ^ " * "
        ^ string_of_int times_to_loop
        ^ " = "
        ^ string_of_int skip_x_rock_drops
        ^ " rock drops, and a height diff of "
        ^ string_of_int (max_height_so_far - max_height_at_time));
      let new_empty_rock_grid =
        Array.make_matrix (Array.length grid) (Array.length grid.(0)) false
      in
      print_endline "Matrix generated";
      let new_cache = ref SMap.empty in
      let new_rock =
        gen_rock rock.id
          {
            x = rock.coordinate.x;
            y = rock.coordinate.y - max_height_so_far + height_of_grid_to_copy;
          }
      in
      set_rock new_empty_rock_grid
        {
          id = 999;
          coordinate = { x = 0; y = 0 };
          grid = grid_to_copy;
          height = height_of_grid_to_copy;
        };
      max_height_so_far + height_in_skip - height_of_grid_to_copy
      + recurse_ticks new_cache new_empty_rock_grid new_rock capped_tick
          wind_flow new_rocks_to_drop height_of_grid_to_copy)
    else
      let new_cache = ref SMap.empty in
      recurse_ticks new_cache grid rock tick wind_flow rocks_to_drop
        max_height_so_far)
  else
    let cur_flow = wind_flow.(capped_tick) in
    loop_detector_cache :=
      SMap.add cache_key (rocks_to_drop, max_height_so_far) !loop_detector_cache;
    let attemped_new_rock =
      match cur_flow with
      | '>' ->
          {
            id = rock.id;
            grid = rock.grid;
            height = rock.height;
            coordinate = { x = rock.coordinate.x + 1; y = rock.coordinate.y };
          }
      | '<' ->
          {
            id = rock.id;
            grid = rock.grid;
            height = rock.height;
            coordinate = { x = rock.coordinate.x - 1; y = rock.coordinate.y };
          }
      | 'v' ->
          {
            id = rock.id;
            grid = rock.grid;
            height = rock.height;
            coordinate = { x = rock.coordinate.x; y = rock.coordinate.y - 1 };
          }
      | _ -> failwith "Impossible wind flow"
    in
    match
      ( local_grid_intersects attemped_new_rock.grid
          (local_grid grid attemped_new_rock.coordinate { x = 4; y = 4 }),
        cur_flow )
    with
    | true, 'v' ->
        (* print_endline "v (blocked)"; *)
        set_rock grid rock;
        (* print_endline (string_of_grid grid); *)
        let new_rocks_to_drop = rocks_to_drop - 1 in
        let new_max_height_so_far =
          max max_height_so_far (rock.coordinate.y + rock.height)
        in
        if new_rocks_to_drop = 0 then new_max_height_so_far
        else
          let new_rock =
            gen_rock (rock.id + 1) { x = 2; y = new_max_height_so_far + 3 }
          in
          recurse_ticks loop_detector_cache grid new_rock (capped_tick + 1)
            wind_flow new_rocks_to_drop new_max_height_so_far
    | true, _ ->
        (* print_char cur_flow;
           print_char ' ';
           print_endline "(blocked)"; *)
        recurse_ticks loop_detector_cache grid rock (capped_tick + 1) wind_flow
          rocks_to_drop max_height_so_far
    | false, _ ->
        (* print_char cur_flow;
           print_endline ""; *)
        recurse_ticks loop_detector_cache grid attemped_new_rock
          (capped_tick + 1) wind_flow rocks_to_drop max_height_so_far

let solve_part input rocks_to_drop grid_width =
  let wind_flow = parse_input input in
  let max_rocks_to_drop_before_loop = 10_000 in
  let rock_grid =
    Array.make_matrix
      ((min max_rocks_to_drop_before_loop rocks_to_drop + 1) * 4)
      grid_width false
  in
  print_endline "Matrix generated";
  let first_rock = gen_rock 0 { x = 2; y = 3 } in
  let loop_detector_cache = ref SMap.empty in
  recurse_ticks loop_detector_cache rock_grid first_rock 0 wind_flow
    rocks_to_drop 0

let part1 input = string_of_int (solve_part input 2022 7)
let part2 input = string_of_int (solve_part input 1_000_000_000_000 7);;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
