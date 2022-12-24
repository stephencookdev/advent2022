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

let parse_value raw_str =
  let str = Str.global_replace (Str.regexp {|[ ]*|}) "" raw_str in
  let op = Str.global_replace (Str.regexp {|[a-z0-9]*|}) "" str in
  if op = "" then ("", '.', "", Some (int_of_string str))
  else
    let split_val = String.split_on_char (String.get op 0) str in
    let left = List.nth split_val 0 in
    let right = List.nth split_val 1 in
    (left, String.get op 0, right, None)

let rec parse_input input map =
  match input with
  | [] -> map
  | x :: xs ->
      let split_val = String.split_on_char ':' x in
      let key = List.nth split_val 0 in
      let value = List.nth split_val 1 in
      parse_input xs (SMap.add key (parse_value value) map)

let rec calc key map =
  let l, op, r, result = SMap.find key map in
  match result with
  | Some x -> x
  | None ->
      let left = calc l map in
      let right = calc r map in
      let res =
        match op with
        | '+' -> left + right
        | '-' -> left - right
        | '*' -> left * right
        | '/' -> left / right
        | _ -> 0
      in
      res

let rec pow x n =
  if n = 0 then 1
  else if n = 1 then x
  else if n mod 2 = 0 then pow (x * x) (n / 2)
  else x * pow (x * x) (n / 2)

(* Hill-climb to find the value of x to make f(x) = 0 *)
let hill_climb f =
  let find_neigbouring_values x =
    List.flatten
      (List.map
         (fun n -> [ x + n; x - n ])
         (List.init 1000 (fun x -> x) @ List.init 100 (fun x -> pow 4 x)))
  in
  let cur_point = ref 0 in
  while f !cur_point != 0 do
    print_endline ("Trying with best point " ^ string_of_int !cur_point);
    let neighbours = find_neigbouring_values !cur_point in
    let best_neighbour =
      List.fold_left
        (fun (best, best_val) n ->
          let n_val = f n in
          print_endline
            ("Trying for n: f(n), " ^ string_of_int n ^ ": "
           ^ string_of_int n_val);
          if (n_val = best_val && abs n < abs best) || n_val > best_val then
            (n, n_val)
          else (best, best_val))
        (!cur_point, f !cur_point)
        neighbours
    in
    cur_point := fst best_neighbour
  done;
  !cur_point

let part1 input =
  let map = parse_input input SMap.empty in
  calc "root" map

let part2 input =
  let map = parse_input input SMap.empty in
  let fixed_map_for_root =
    SMap.update "root"
      (fun existing_root ->
        match existing_root with
        | None -> failwith "root not found"
        | Some (l, _, r, _) -> Some (l, '-', r, None))
      map
  in
  hill_climb (fun n ->
      let fixed_map_for_humn =
        SMap.update "humn"
          (fun _ -> Some ("", '.', "", Some n))
          fixed_map_for_root
      in
      -abs (calc "root" fixed_map_for_humn))
;;

print_endline "";
print_endline ("Part 1: " ^ Batteries.dump (part1 read_input));
print_endline ("Part 2: " ^ Batteries.dump (part2 read_input))
