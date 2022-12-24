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

type node = {
  value : int;
  next : node option ref;
  prev : node option ref;
  moved : bool ref;
  original_index : int;
}

let rec for_each_node f node len =
  match len with
  | 0 -> ()
  | _ ->
      f node;
      for_each_node f (Option.get !(node.next)) (len - 1)

let parse_input ns decryption_key =
  let int_list =
    List.mapi (fun i n -> (int_of_string n * decryption_key, i)) ns
  in
  let init_node =
    {
      value = 0;
      next = ref None;
      prev = ref None;
      moved = ref false;
      original_index = -1;
    }
  in
  let tl_node =
    List.fold_left
      (fun prev_node (n, i) ->
        let new_node =
          {
            value = n;
            next = ref None;
            prev = ref (Some prev_node);
            moved = ref false;
            original_index = i;
          }
        in
        prev_node.next := Some new_node;
        new_node)
      init_node int_list
  in
  let first_node = Option.get !(init_node.next) in
  first_node.prev := Some tl_node;
  tl_node.next := Some first_node;
  (first_node, List.length int_list)

let rec nth_node node raw_n len =
  match raw_n mod len with
  | 0 -> node
  | n ->
      if n > 0 then nth_node (Option.get !(node.next)) (n - 1) len
      else nth_node (Option.get !(node.prev)) (n + 1) len

let rec find_node f node try_count =
  match try_count with
  | 0 -> raise Not_found
  | _ ->
      if f node.value then node
      else find_node f (Option.get !(node.next)) (try_count - 1)

let rec move_next_n node len =
  let node_to_move_ref = ref node in
  let best_node_to_move_ref = ref node in
  let nodes_looked_at = ref 0 in
  while !nodes_looked_at < len do
    nodes_looked_at := !nodes_looked_at + 1;
    let cur_node = Option.get !(!node_to_move_ref.next) in
    node_to_move_ref := cur_node;
    if
      !(!best_node_to_move_ref.moved)
      || cur_node.original_index < !best_node_to_move_ref.original_index
         && not !(cur_node.moved)
    then best_node_to_move_ref := cur_node
  done;
  let node_to_move = !best_node_to_move_ref in
  (* print_endline ("Moving " ^ string_of_int node_to_move.value); *)
  if not !(node_to_move.moved) then (
    let old_prev = Option.get !(node_to_move.prev) in
    let old_next = Option.get !(node_to_move.next) in
    old_prev.next := Some old_next;
    old_next.prev := Some old_prev;

    let new_prev = nth_node old_prev node_to_move.value (len - 1) in
    let new_next = Option.get !(new_prev.next) in
    new_prev.next := Some node_to_move;
    new_next.prev := Some node_to_move;
    node_to_move.prev := Some new_prev;
    node_to_move.next := Some new_next;

    (* for_each_node (fun n -> print_endline (string_of_int n.value)) node len;
       print_endline ""; *)
    node_to_move.moved := true;
    move_next_n node_to_move len)

let part1 input =
  let n_loop_hd, n_loop_len = parse_input input 1 in
  move_next_n n_loop_hd n_loop_len;
  let n_loop_0_node = find_node (fun n -> n = 0) n_loop_hd n_loop_len in
  let n1, n2, n3 =
    ( nth_node n_loop_0_node 1000 n_loop_len,
      nth_node n_loop_0_node 2000 n_loop_len,
      nth_node n_loop_0_node 3000 n_loop_len )
  in
  n1.value + n2.value + n3.value

let part2 input =
  let n_loop_hd, n_loop_len = parse_input input 811589153 in
  for i = 1 to 10 do
    ignore i;
    (* print_endline ("Move " ^ string_of_int i ^ ":"); *)
    for_each_node (fun n -> n.moved := false) n_loop_hd n_loop_len;
    move_next_n n_loop_hd n_loop_len
  done;
  let n_loop_0_node = find_node (fun n -> n = 0) n_loop_hd n_loop_len in
  let n1, n2, n3 =
    ( nth_node n_loop_0_node 1000 n_loop_len,
      nth_node n_loop_0_node 2000 n_loop_len,
      nth_node n_loop_0_node 3000 n_loop_len )
  in
  n1.value + n2.value + n3.value
;;

print_endline "";
print_endline ("Part 1: " ^ Batteries.dump (part1 read_input));
print_endline ("Part 2: " ^ Batteries.dump (part2 read_input))
