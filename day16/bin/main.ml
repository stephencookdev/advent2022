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

type node = { flow_rate : int; next_steps : (string * int) list }

module SMap = Map.Make (String)

let sub_to_end s n = String.sub s n (String.length s - n)

let rec get_best_from_to a b prog_map_ref map important_nodes max_depth =
  if a = b then 0
  else if max_depth = 0 then raise Not_found
  else
    let key = String.concat "," (List.sort String.compare [ a; b ]) in
    if SMap.mem key !prog_map_ref then SMap.find key !prog_map_ref
    else
      let result =
        let cur = SMap.find a map in
        let next_steps = cur.next_steps in
        let nested_next_steps =
          (* if trying to go from A->Z, and the best way is through P
             then we always want to list A->P and P->Z as the edges,
             rather than A->P, P->Z, and A->Z, since this reduces a lot
             of duplication that is needless in the graph *)
          List.filter
            (fun (s, _) -> not (List.mem s important_nodes))
            next_steps
        in
        try
          let _, t = List.find (fun (s, _) -> s = b) next_steps in
          t
        with Not_found ->
          List.fold_left min 9000
            (List.map
               (fun (s, t) ->
                 try
                   t
                   + get_best_from_to s b prog_map_ref map important_nodes
                       (max_depth - 1)
                 with Not_found -> 9000)
               nested_next_steps)
      in
      if result < 9000 then prog_map_ref := SMap.add key result !prog_map_ref;
      result

let simplify_map old_map start_nodes open_nodes max_depth =
  let important_nodes =
    List.sort_uniq String.compare
      (List.append start_nodes
         (List.map
            (fun (node_name, _) -> node_name)
            (List.filter
               (fun (node_name, node) ->
                 node.flow_rate > 0 && not (List.mem node_name open_nodes))
               (List.of_seq (SMap.to_seq old_map)))))
  in
  let best_from_to_map = ref SMap.empty in
  let best_from_to =
    List.flatten
      (List.map
         (fun from_node ->
           List.map
             (fun to_node ->
               ( from_node,
                 to_node,
                 get_best_from_to from_node to_node best_from_to_map old_map
                   important_nodes max_depth ))
             important_nodes)
         important_nodes)
  in
  let best_from_to_no_empty =
    List.filter (fun (_, _, d) -> d > 0 && d < 9000) best_from_to
  in
  List.fold_left
    (fun map (from_node, to_node, distance) ->
      let cur_old_map_entry = SMap.find from_node old_map in
      let flow_rate = cur_old_map_entry.flow_rate in
      if SMap.mem from_node map then
        let cur_map_entry = SMap.find from_node map in
        SMap.add from_node
          {
            flow_rate;
            next_steps = (to_node, distance) :: cur_map_entry.next_steps;
          }
          (SMap.remove from_node map)
      else
        SMap.add from_node
          { flow_rate; next_steps = [ (to_node, distance) ] }
          map)
    SMap.empty best_from_to_no_empty

let parse_map_line raw_map_line =
  let node = String.sub raw_map_line 6 2 in
  let flow_rate =
    int_of_string
      (Str.global_replace (Str.regexp {|;.*|}) "" (sub_to_end raw_map_line 23))
  in
  let raw_next_steps =
    Str.global_replace (Str.regexp {| |}) ""
      (Str.global_replace
         (Str.regexp {|.*tunnels? leads? to valves? |})
         "" raw_map_line)
  in
  let next_steps =
    List.map (fun s -> (s, 1)) (String.split_on_char ',' raw_next_steps)
  in
  (node, flow_rate, next_steps)

let parse_map input =
  List.fold_left
    (fun cur_map raw_map_line ->
      let node, flow_rate, next_steps = parse_map_line raw_map_line in
      SMap.add node { flow_rate; next_steps } cur_map)
    SMap.empty input

let open_current_node map node time =
  (time - 1) * (SMap.find node map).flow_rate

let get_possible_steps map node = (SMap.find node map).next_steps

let rec get_all_combos list_of_lists =
  match list_of_lists with
  | [] -> []
  | l :: [] -> List.map (fun x -> [ x ]) l
  | l :: ls ->
      List.flatten
        (List.map (fun x -> List.map (fun y -> x :: y) (get_all_combos ls)) l)

let rec traverse_map map worker_nodes open_nodes worker_time solution_keeper
    map_cache =
  let key =
    String.concat ","
      (List.map2
         (fun node time -> node ^ "_" ^ string_of_int time)
         worker_nodes worker_time)
    ^ ","
    ^ String.concat "," (List.sort String.compare open_nodes)
  in
  if List.exists (fun t -> t < 0) worker_time then min_int
  else if List.for_all (fun t -> t = 0) worker_time then 0
  else if SMap.is_empty map then 0
  else if SMap.mem key !solution_keeper then SMap.find key !solution_keeper
  else
    let worker_possible_steps =
      List.mapi
        (fun i node ->
          let time = List.nth worker_time i in
          if time <= 0 then
            (* once you've run out of time, just hang around nowhere forever *)
            [ ("__", 1) ]
          else
            let possible_steps = get_possible_steps map node in
            let valve_can_be_opened = not (List.mem node open_nodes) in
            let valve_value = (SMap.find node map).flow_rate in
            if valve_can_be_opened && valve_value > 0 then
              (node, 0) :: possible_steps
            else possible_steps)
        worker_nodes
    in
    let potential_new_worker_nodes_raw = get_all_combos worker_possible_steps in
    let potential_new_worker_nodes =
      List.filter
        (fun new_worker_nodes ->
          let is_invalid_multi_open =
            let repeat_nodes =
              List.map
                (fun (s, _) -> s)
                (List.filter (fun (_, t) -> t = 0) new_worker_nodes)
            in
            let unique_repeat_nodes =
              List.sort_uniq String.compare repeat_nodes
            in
            List.length unique_repeat_nodes = List.length repeat_nodes
          in
          is_invalid_multi_open)
        potential_new_worker_nodes_raw
    in
    let rec h (node_name, t) depth =
      if node_name = "__" || depth = 0 then 0
      else
        let node = SMap.find node_name map in
        (node.flow_rate / ((t + 1) * (t + 1)))
        + List.fold_left ( + ) 0
            (List.map
               (fun n -> h n (depth - 1))
               (List.filter
                  (fun (s, _) ->
                    not (List.mem s open_nodes || List.mem s worker_nodes))
                  node.next_steps))
    in
    let node_score_h n = h n 1 in
    let string_of_nwn nwn =
      String.concat "."
        (List.sort_uniq String.compare (List.map (fun (s, _) -> s) nwn))
    in
    let heuristic_potential_new_worker_nodes =
      List.filteri
        (fun i _ -> i < 4)
        (List.sort
           (fun nwn1 nwn2 ->
             List.fold_left ( + ) 0 (List.map node_score_h nwn2)
             - List.fold_left ( + ) 0 (List.map node_score_h nwn1))
           (List.sort_uniq
              (fun nwn1 nwn2 ->
                String.compare (string_of_nwn nwn1) (string_of_nwn nwn2))
              potential_new_worker_nodes))
    in
    let branches =
      List.map
        (fun raw_new_worker_nodes ->
          let nodes_to_open =
            List.map
              (fun (s, _) -> s)
              (List.filter
                 (fun (s, t) -> s != "__" && t = 0)
                 raw_new_worker_nodes)
          in
          let new_open_nodes = List.append nodes_to_open open_nodes in
          let valve_values =
            List.mapi
              (fun i (s, t) ->
                let time = List.nth worker_time i in
                if t = 0 then open_current_node map s time else 0)
              raw_new_worker_nodes
          in
          let total_valve_value = List.fold_left ( + ) 0 valve_values in
          let new_worker_nodes =
            List.map (fun (s, _) -> s) raw_new_worker_nodes
          in
          let new_worker_time =
            List.mapi
              (fun i (_, t) ->
                let time = List.nth worker_time i in
                let time_to_subtract = if t = 0 then 1 else t in
                time - time_to_subtract)
              raw_new_worker_nodes
          in
          total_valve_value
          + traverse_map map new_worker_nodes new_open_nodes new_worker_time
              solution_keeper map_cache)
        heuristic_potential_new_worker_nodes
    in
    let result = List.fold_left max 0 branches in
    solution_keeper := SMap.add key result !solution_keeper;
    result

let solve_part input time workers =
  let start = "AA" in
  let map = simplify_map (parse_map input) [ start ] [] time in
  let solution_keeper = ref SMap.empty in
  let map_cache = ref SMap.empty in
  traverse_map map
    (List.init workers (fun _ -> start))
    []
    (List.init workers (fun _ -> time))
    solution_keeper map_cache

let part1 input = string_of_int (solve_part input 30 1)
let part2 input = string_of_int (solve_part input 26 2);;

print_endline "";
print_endline ("Part 1: " ^ part1 read_input);
print_endline ("Part 2: " ^ part2 read_input)
