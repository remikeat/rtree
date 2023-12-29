open Printf

type point = { x : float; y : float }
type bounding_box = { min : point; max : point }

let get_bounding_box lst =
  let min_x, min_y, max_x, max_y =
    match lst with
    | [] -> raise Not_found
    | hd :: tl ->
      let initial_values = (hd.x, hd.y, hd.x, hd.y) in
      tl
      |> List.fold_left
           (fun (min_x, min_y, max_x, max_y) v ->
             (min min_x v.x, min min_y v.y, max max_x v.x, max max_y v.y))
           initial_values
  in
  { min = { x = min_x; y = min_y }; max = { x = max_x; y = max_y } }

let bb_str bb =
  sprintf "\"bb\" : [%.2f, %.2f, %.2f, %.2f]" bb.min.x bb.min.y bb.max.x
    bb.max.y

let get_area bb = (bb.max.x -. bb.min.x) *. (bb.max.y -. bb.min.y)

let get_union bb_a bb_b =
  {
    min = { x = min bb_a.min.x bb_b.min.x; y = min bb_a.min.y bb_b.min.y };
    max = { x = max bb_a.max.x bb_b.max.x; y = max bb_a.max.y bb_b.max.y };
  }

module type DataType = sig
  type t

  val val_to_string : t -> string
  val min_children : int
  val max_children : int
end

module Make (Data : DataType) = struct
  type t = Data.t

  type r_tree_node = Node of r_tree list | Leaf of t
  and r_tree = { bb : bounding_box; node : r_tree_node }

  let init bb v = { bb; node = Node [ { bb; node = Leaf v } ] }

  let choose_leaf tree bb =
    let find_min children =
      match children with
      | [] -> None
      | hd :: tl ->
        let first_area = get_area (get_union hd.bb bb) in
        let _, min_idx, _ =
          tl
          |> List.fold_left
               (fun (min_val, min_idx, cur_idx) child ->
                 let cur_area = get_area (get_union child.bb bb) in
                 if cur_area < min_val then (cur_area, cur_idx, cur_idx + 1)
                 else (min_val, min_idx, cur_idx + 1))
               (first_area, 0, 1)
        in
        Some min_idx
    in

    let rec choose_leaf' node acc =
      match node.node with
      | Leaf _ -> acc |> List.rev
      | Node children -> (
        match find_min children with
        | None -> acc |> List.rev
        | Some min_idx ->
          choose_leaf' (List.nth children min_idx) (min_idx :: acc))
    in
    choose_leaf' tree []

  let update n v lst =
    let rec update' lst acc i =
      match lst with
      | [] -> acc |> List.rev
      | hd :: tl ->
        if i = n then update' tl (v :: acc) (i + 1)
        else update' tl (hd :: acc) (i + 1)
    in
    update' lst [] 0

  let split n lst =
    let rec split' lst acc i =
      if i < n then
        match lst with
        | [] -> (acc |> List.rev, [])
        | hd :: tl -> split' tl (hd :: acc) (i + 1)
      else (acc |> List.rev, lst)
    in
    split' lst [] 0

  let get_item_pair item lst =
    let rec get_item_pair' lst acc =
      match lst with
      | [] -> acc |> List.rev
      | hd :: tl -> get_item_pair' tl ((item, hd) :: acc)
    in
    get_item_pair' lst []

  let get_all_pairs lst =
    let rec get_all_pairs' lst acc =
      match lst with
      | [] -> acc
      | hd :: tl -> get_all_pairs' tl (acc @ get_item_pair hd tl)
    in
    get_all_pairs' lst []

  let find_most_wasteful_pair lst =
    let idx_lst = List.init (List.length lst) (fun x -> x) in
    let _, pair =
      match idx_lst |> get_all_pairs with
      | [] -> raise Not_found
      | (hd1, hd2) :: tl ->
        let hd1_bb = (List.nth lst hd1).bb in
        let hd2_bb = (List.nth lst hd2).bb in
        let init_val = get_area (get_union hd1_bb hd2_bb) in
        tl
        |> List.fold_left
             (fun (max_val, max_pair) (cur_pair1, cur_pair2) ->
               let cur_pair1_bb = (List.nth lst cur_pair1).bb in
               let cur_pair2_bb = (List.nth lst cur_pair2).bb in
               let area = get_area (get_union cur_pair1_bb cur_pair2_bb) in
               if max_val < area then (area, (cur_pair1, cur_pair2))
               else (max_val, max_pair))
             (init_val, (hd1, hd2))
    in
    pair

  let inside_list i idx_lst = List.exists (fun v -> v = i) idx_lst

  let remove_idx lst idx_lst =
    let rec remove_idx' lst acc i =
      match lst with
      | [] -> acc |> List.rev
      | hd :: tl ->
        let acc = if inside_list i idx_lst then acc else hd :: acc in
        remove_idx' tl acc (i + 1)
    in
    remove_idx' lst [] 0

  let get_bb node_lst =
    match node_lst with
    | [] -> raise Not_found
    | hd :: tl -> tl |> List.fold_left (fun acc v -> get_union acc v.bb) hd.bb

  let split_by_area lst =
    let node_1, node_2 = find_most_wasteful_pair lst in
    let new_lst = remove_idx lst [ node_1; node_2 ] in
    new_lst
    |> List.fold_left
         (fun (lst_1, lst_2) v ->
           let lst1_bb = get_bb lst_1 in
           let lst2_bb = get_bb lst_2 in
           let calc_increase_area lst_bb =
             get_area (get_union lst_bb v.bb) -. get_area lst_bb
           in
           if calc_increase_area lst1_bb < calc_increase_area lst2_bb then
             (v :: lst_1, lst_2)
           else (lst_1, v :: lst_2))
         ([ List.nth lst node_1 ], [ List.nth lst node_2 ])

  let update_nth_children cur_node idx mod_node =
    match cur_node.node with
    | Leaf _ -> raise Not_found
    | Node children ->
      {
        bb = get_union cur_node.bb mod_node.bb;
        node = Node (update idx mod_node children);
      }

  let get_node_and_parents_lst tree path =
    path
    |> List.fold_left
         (fun (acc, acc_lst) v ->
           match acc.node with
           | Leaf _ -> raise Not_found
           | Node children -> (List.nth children v, (v, acc) :: acc_lst))
         (tree, [])

  let update_node_from_parents_lst lst mod_node =
    lst
    |> List.fold_left
         (fun acc (idx, v) -> update_nth_children v idx acc)
         mod_node

  let update_node tree path mod_node =
    let _, lst = get_node_and_parents_lst tree path in
    update_node_from_parents_lst lst mod_node

  let insert_children parent node =
    match parent.node with
    | Leaf _ -> raise Not_found
    | Node children ->
      let new_children = node :: (children |> List.rev) |> List.rev in
      { bb = get_union parent.bb node.bb; node = Node new_children }

  let nb_children parent =
    match parent.node with
    | Leaf _ -> raise Not_found
    | Node children -> children |> List.length

  let split_node parent node =
    match parent.node with
    | Leaf _ -> raise Not_found
    | Node children ->
      let child_1, child_2 =
        split_by_area (node :: (children |> List.rev) |> List.rev)
      in
      {
        bb = get_union parent.bb node.bb;
        node =
          Node
            [
              { bb = get_bb child_1; node = Node child_1 };
              { bb = get_bb child_2; node = Node child_2 };
            ];
      }

  let insert_value tree bb v =
    let path = choose_leaf tree bb in
    let _, lst = get_node_and_parents_lst tree path in
    match lst with
    | [] -> raise Not_found
    | (_, parent) :: tl ->
      let new_parent =
        if nb_children parent < Data.max_children then
          insert_children parent { bb; node = Leaf v }
        else split_node parent { bb; node = Leaf v }
      in
      update_node_from_parents_lst tl new_parent

  let sprint_tree tree =
    let spaces level = String.make (2 * level) ' ' in
    let rec sprint_tree' node level =
      match node.node with
      | Leaf v ->
        sprintf "%s {\"id\" : %s, %s}" (spaces level) (Data.val_to_string v)
          (bb_str node.bb)
      | Node children ->
        sprintf "%s{%s, " (spaces level) (bb_str node.bb)
        ^ "\"children\": [\n"
        ^ (children
          |> List.map (fun v -> sprintf "%s" (sprint_tree' v (level + 1)))
          |> String.concat ",\n")
        ^ sprintf "\n%s]}" (spaces level)
    in
    sprint_tree' tree 0
end
