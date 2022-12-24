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

type monkey = {
  rules : int -> int -> int * int;
  to_inspect : int list;
  inspections : int;
  divisible_test : int;
  lcm : int;
}

let rec split_to_last list =
  match list with
  | [] -> failwith "No last exists"
  | x :: [] -> ([], x)
  | x :: xs ->
      let h, t = split_to_last xs in
      (x :: h, t)

let rec parse_monkeys_raw monkeys_raw input =
  match input with
  | [] -> monkeys_raw
  | x :: xs ->
      if x = "" then parse_monkeys_raw (List.append monkeys_raw [ [] ]) xs
      else
        let h, t = split_to_last monkeys_raw in
        parse_monkeys_raw (List.append h [ List.append t [ x ] ]) xs

let sub_to_end s n = String.sub s n (String.length s - n)

let parse_monkey monkey_raw worry_dampener =
  let to_inspect_raw =
    Str.global_replace (Str.regexp {|[^0-9,]|}) "" (List.nth monkey_raw 1)
  in
  let to_inspect =
    List.map int_of_string (String.split_on_char ',' to_inspect_raw)
  in
  let operation_raw =
    String.split_on_char ' ' (sub_to_end (List.nth monkey_raw 2) 19)
  in
  let x, op, y =
    ( List.nth operation_raw 0,
      List.nth operation_raw 1,
      List.nth operation_raw 2 )
  in
  let operation n =
    let x_int = if x = "old" then n else int_of_string x in
    let y_int = if y = "old" then n else int_of_string y in
    match op with
    | "+" -> x_int + y_int
    | "-" -> x_int - y_int
    | "*" -> x_int * y_int
    | _ -> failwith "Unknown op"
  in
  let divisible_test = int_of_string (sub_to_end (List.nth monkey_raw 3) 21) in
  let to_true_monkey = int_of_string (sub_to_end (List.nth monkey_raw 4) 29) in
  let to_false_monkey = int_of_string (sub_to_end (List.nth monkey_raw 5) 30) in
  {
    to_inspect;
    divisible_test;
    lcm = 0;
    inspections = 0;
    rules =
      (fun n lcm ->
        let new_n = operation n mod lcm / worry_dampener in
        if new_n mod divisible_test = 0 then (new_n, to_true_monkey)
        else (new_n, to_false_monkey));
  }

(* cba to work out lcm properly, this is _a_ common multiple... *)
let rec find_lcm ns = match ns with [] -> 1 | n :: ns -> n * find_lcm ns

let parse_input (input : string list) (worry_dampener : int) : monkey list =
  let monkeys_raw = parse_monkeys_raw [ [] ] input in
  let monkeys_parsed =
    List.map (fun m -> parse_monkey m worry_dampener) monkeys_raw
  in
  let lcm = find_lcm (List.map (fun m -> m.divisible_test) monkeys_parsed) in
  List.map
    (fun m ->
      {
        lcm;
        rules = m.rules;
        to_inspect = m.to_inspect;
        inspections = m.inspections;
        divisible_test = m.divisible_test;
      })
    monkeys_parsed

let apply_monkey_rules to_inspect rules lcm =
  List.map (fun n -> rules n lcm) to_inspect

let new_inspects_for_monkey_n new_inspects n =
  let specific_results =
    List.filter (fun (_, monkey_n) -> monkey_n = n) new_inspects
  in
  List.map (fun (id, _) -> id) specific_results

let apply_single_monkey (i : int) (monkeys : monkey list) : monkey list =
  let current_monkey = List.nth monkeys i in
  let new_inspect_homes =
    apply_monkey_rules current_monkey.to_inspect current_monkey.rules
      current_monkey.lcm
  in
  List.mapi
    (fun j monkey ->
      {
        to_inspect =
          (if i = j then []
          else
            List.append monkey.to_inspect
              (new_inspects_for_monkey_n new_inspect_homes j));
        inspections =
          (if i = j then monkey.inspections + List.length monkey.to_inspect
          else monkey.inspections);
        rules = monkey.rules;
        lcm = monkey.lcm;
        divisible_test = monkey.divisible_test;
      })
    monkeys

let apply_monkeys_single_round monkeys =
  let after_monkeys = ref monkeys in
  for i = 0 to List.length monkeys - 1 do
    after_monkeys := apply_single_monkey i !after_monkeys
  done;
  !after_monkeys

let rec apply_monkeys n monkeys =
  match n with
  | 0 -> monkeys
  | n -> apply_monkeys (n - 1) (apply_monkeys_single_round monkeys)

let find_top_inspections monkeys =
  let inspections = List.map (fun monkey -> monkey.inspections) monkeys in
  List.sort (fun a b -> b - a) inspections

let take2 list = (List.nth list 0, List.nth list 1)

let solve_part input n worry_dampener =
  let start_monkeys = parse_input input worry_dampener in
  let end_monkeys = apply_monkeys n start_monkeys in
  let top_inspections = find_top_inspections end_monkeys in
  let a, b = take2 top_inspections in
  a * b

let part1 input = string_of_int (solve_part input 20 3)
let part2 input = string_of_int (solve_part input 10000 1);;

print_endline "";;
print_endline ("Part 1: " ^ part1 read_input);;
print_endline ("Part 2: " ^ part2 read_input)
