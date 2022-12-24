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

type resources = { ore : int; clay : int; obsidian : int; geode : int }
type robot = { cost : resources }
type blueprint = { ore : robot; clay : robot; obsidian : robot; geode : robot }

let gen_robot raw_robot_str =
  let ore =
    try
      ignore
        (Str.search_forward (Str.regexp {|\([0-9]+\) ore|}) raw_robot_str 0);
      int_of_string (Str.matched_group 1 raw_robot_str)
    with Not_found -> 0
  in
  let clay =
    try
      ignore
        (Str.search_forward (Str.regexp {|\([0-9]+\) clay|}) raw_robot_str 0);
      int_of_string (Str.matched_group 1 raw_robot_str)
    with Not_found -> 0
  in
  let obsidian =
    try
      ignore
        (Str.search_forward
           (Str.regexp {|\([0-9]+\) obsidian|})
           raw_robot_str 0);
      int_of_string (Str.matched_group 1 raw_robot_str)
    with Not_found -> 0
  in
  let geode =
    try
      ignore
        (Str.search_forward (Str.regexp {|\([0-9]+\) geode|}) raw_robot_str 0);
      int_of_string (Str.matched_group 1 raw_robot_str)
    with Not_found -> 0
  in
  { cost = { ore; clay; obsidian; geode } }

let parse_individual_blueprint raw_blueprint =
  ignore
    (Str.search_forward
       (Str.regexp {|Each ore robot \([^\.]+\)|})
       raw_blueprint 0);
  let raw_ore_robot = Str.matched_group 1 raw_blueprint in
  ignore
    (Str.search_forward
       (Str.regexp {|Each clay robot \([^\.]+\)|})
       raw_blueprint 0);
  let raw_clay_robot = Str.matched_group 1 raw_blueprint in
  ignore
    (Str.search_forward
       (Str.regexp {|Each obsidian robot \([^\.]+\)|})
       raw_blueprint 0);
  let raw_obsidian_robot = Str.matched_group 1 raw_blueprint in
  ignore
    (Str.search_forward
       (Str.regexp {|Each geode robot \([^\.]+\)|})
       raw_blueprint 0);
  let raw_geode_robot = Str.matched_group 1 raw_blueprint in
  {
    ore = gen_robot raw_ore_robot;
    clay = gen_robot raw_clay_robot;
    obsidian = gen_robot raw_obsidian_robot;
    geode = gen_robot raw_geode_robot;
  }

let rec parse_blueprints input =
  match input with
  | [] -> []
  | raw_blueprint :: remaining_blueprints ->
      parse_individual_blueprint raw_blueprint
      :: parse_blueprints remaining_blueprints

let rec apply_actions (action_list : (resources * resources) list)
    (robots : resources) (resources : resources) =
  match action_list with
  | [] -> (robots, resources)
  | (robot_diff, resource_diff) :: remaining_actions ->
      apply_actions remaining_actions
        {
          ore = robots.ore + robot_diff.ore;
          clay = robots.clay + robot_diff.clay;
          obsidian = robots.obsidian + robot_diff.obsidian;
          geode = robots.geode + robot_diff.geode;
        }
        {
          ore = resources.ore + robots.ore + resource_diff.ore;
          clay = resources.clay + robots.clay + resource_diff.clay;
          obsidian =
            resources.obsidian + robots.obsidian + resource_diff.obsidian;
          geode = resources.geode + robots.geode + resource_diff.geode;
        }

let turn_to_action (robots_diff : resources) (cost : resources) :
    resources * resources =
  ( robots_diff,
    {
      ore = -cost.ore;
      clay = -cost.clay;
      obsidian = -cost.obsidian;
      geode = -cost.geode;
    } )

(* generate all unique variations of length 1 to n
   for example `generate_variations [1;2;3] 2` would
   return `[[1];[2];[3];[1;2];[1;3];[2;3]]` *)
let generate_variations (list : 'a list) (n : int) : 'a list list =
  let rec generate_variations' (list : 'a list) (n : int) : 'a list list =
    match n with
    | 0 -> []
    | 1 -> List.map (fun x -> [ x ]) list
    | _ ->
        let rec generate_variations'' (list : 'a list) (n : int) : 'a list list
            =
          match list with
          | [] -> []
          | x :: xs ->
              let variations = generate_variations' xs (n - 1) in
              let variations_with_x =
                List.map (fun variation -> x :: variation) variations
              in
              variations_with_x @ generate_variations'' xs n
        in
        generate_variations'' list n
  in
  let rec generate_variations''' (list : 'a list) (n : int) : 'a list list =
    match n with
    | 0 -> []
    | _ -> generate_variations' list n @ generate_variations''' list (n - 1)
  in
  generate_variations''' list n

let rec is_possible (action_list : (resources * resources) list)
    (resource : resources) : bool =
  match action_list with
  | [] -> true
  | (_, resource_diff) :: rest_of_action_list ->
      let remaining_resource : resources =
        {
          ore = resource.ore + resource_diff.ore;
          clay = resource.clay + resource_diff.clay;
          obsidian = resource.obsidian + resource_diff.obsidian;
          geode = resource.geode + resource_diff.geode;
        }
      in
      remaining_resource.ore >= 0
      && remaining_resource.clay >= 0
      && remaining_resource.obsidian >= 0
      && remaining_resource.geode >= 0
      && is_possible rest_of_action_list remaining_resource

let generate_actions blueprint (resource : resources) =
  let build_ore_robot =
    turn_to_action
      { ore = 1; clay = 0; obsidian = 0; geode = 0 }
      blueprint.ore.cost
  in
  let build_clay_robot =
    turn_to_action
      { ore = 0; clay = 1; obsidian = 0; geode = 0 }
      blueprint.clay.cost
  in
  let build_obsidian_robot =
    turn_to_action
      { ore = 0; clay = 0; obsidian = 1; geode = 0 }
      blueprint.obsidian.cost
  in
  let build_geode_robot =
    turn_to_action
      { ore = 0; clay = 0; obsidian = 0; geode = 1 }
      blueprint.geode.cost
  in
  let build_no_robots =
    turn_to_action
      { ore = 0; clay = 0; obsidian = 0; geode = 0 }
      { ore = 0; clay = 0; obsidian = 0; geode = 0 }
  in
  let all_possible_individual_actions =
    [
      build_ore_robot;
      build_clay_robot;
      build_obsidian_robot;
      build_geode_robot;
      build_no_robots;
    ]
  in
  let all_variations = generate_variations all_possible_individual_actions 1 in
  List.filter
    (fun action_list -> is_possible action_list resource)
    all_variations

let rec take n list =
  match (n, list) with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs

let add_resources (r1 : resources) (r2 : resources) : resources =
  {
    ore = r1.ore + r2.ore;
    clay = r1.clay + r2.clay;
    obsidian = r1.obsidian + r2.obsidian;
    geode = r1.geode + r2.geode;
  }

let get_score_ratio blueprint : resources =
  let spread =
    List.fold_left add_resources
      { ore = 0; clay = 0; obsidian = 0; geode = 0 }
      [
        blueprint.ore.cost;
        blueprint.clay.cost;
        blueprint.obsidian.cost;
        blueprint.geode.cost;
      ]
  in
  {
    ore = spread.ore * 1;
    clay = spread.clay * 2;
    obsidian = spread.obsidian * 3;
    geode = max_int;
  }

let rec collect_geodes (blueprint : blueprint) (current_robots : resources)
    (current_resources : resources) time =
  if time = 0 then current_resources.geode
  else
    let score_ratio = get_score_ratio blueprint in
    let actions_variations = generate_actions blueprint current_resources in
    let compare_actions_variations av1 av2 =
      let rec actions_variation_heuristic_score
          (av_list : (resources * resources) list) : int =
        match av_list with
        | [] -> 0
        | (robots, _) :: rest_of_av_list ->
            (robots.ore * score_ratio.ore)
            + (robots.clay * score_ratio.clay)
            + (robots.obsidian * score_ratio.obsidian)
            + (robots.geode * score_ratio.geode)
            + actions_variation_heuristic_score rest_of_av_list
      in
      actions_variation_heuristic_score av2
      - actions_variation_heuristic_score av1
    in
    let ranked_actions_variations =
      List.sort compare_actions_variations actions_variations
    in
    let new_states : (resources * resources) list =
      List.map
        (fun actions -> apply_actions actions current_robots current_resources)
        (take 2 ranked_actions_variations)
    in
    let possible_geodes =
      List.map
        (fun (new_robots, new_resources) ->
          collect_geodes blueprint new_robots new_resources (time - 1))
        new_states
    in
    List.fold_left max 0 possible_geodes

let part1 input =
  let blueprints = parse_blueprints input in
  let results =
    List.map
      (fun b ->
        collect_geodes b
          { ore = 1; clay = 0; obsidian = 0; geode = 0 }
          { ore = 0; clay = 0; obsidian = 0; geode = 0 }
          24)
      blueprints
  in
  print_endline (Batteries.dump results);
  let quality_levels = List.mapi (fun i result -> (i + 1) * result) results in
  List.fold_left ( + ) 0 quality_levels

let part2 input =
  let blueprints = take 3 (parse_blueprints input) in
  let results =
    List.map
      (fun b ->
        collect_geodes b
          { ore = 1; clay = 0; obsidian = 0; geode = 0 }
          { ore = 0; clay = 0; obsidian = 0; geode = 0 }
          32)
      blueprints
  in
  print_endline (Batteries.dump results);
  List.fold_left ( * ) 1 results
;;

print_endline "";
print_endline ("Part 1: " ^ Batteries.dump (part1 read_input));
print_endline ("Part 2: " ^ Batteries.dump (part2 read_input))
