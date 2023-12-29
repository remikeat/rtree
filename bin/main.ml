open Rtree
open Yojson.Basic.Util
(* open Printf *)

module IntRTree = Rtree.Make (struct
  type t = int

  let val_to_string = Printf.sprintf "%i"
  let min_children = 1
  let max_children = 3
end)

let json = Yojson.Basic.from_file "convex_hulls.json"

let json_to_convex_hull_bb json =
  json |> member "apexes" |> to_list
  |> List.map (fun v ->
         { x = v |> member "x" |> to_float; y = v |> member "y" |> to_float })
  |> Rtree.get_bounding_box

let bb_lst =
  json |> member "convex hulls" |> to_list
  |> List.map (fun v -> v |> json_to_convex_hull_bb)

let tree =
  match bb_lst with
  | [] -> raise Not_found
  | hd :: tl ->
    let init_tree = IntRTree.init hd 0 in
    let tree, _ =
      tl
      |> List.fold_left
           (fun (acc, idx) v -> (IntRTree.insert_value acc v idx, idx + 1))
           (init_tree, 0)
    in
    tree

let () = tree |> IntRTree.sprint_tree |> print_endline
