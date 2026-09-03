(* Definition of type tree *)
type tree = 
  | EmptyTree
  | Node of int * tree * tree;;

(* Declaration of example_tree *)

let example_tree = 
  Node (1,
        Node (2,
              Node (4, EmptyTree, EmptyTree),
              Node (5,
                    Node (7, EmptyTree, EmptyTree),
                    EmptyTree)),
        Node (3,
              EmptyTree,
              Node (6, EmptyTree, EmptyTree))
       );;

(* Definition of nodes_with_no_ltree_filtered *)

let rec nodes_with_no_ltree_filtered t lnums =
  match t with
  | EmptyTree -> []
  | Node (v, l, r) ->
      let left_result = nodes_with_no_ltree_filtered l lnums in
      let right_result = nodes_with_no_ltree_filtered r lnums in
      (* Check if the left child is EmptyTree and if v ∈ lnums *)
      let current =
        if l = EmptyTree && List.mem v lnums then [v]
        else []
      in
      left_result @ current @ right_result;;

(* Test calls for nodes_with_no_ltree_filtered *)
nodes_with_no_ltree_filtered example_tree [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7];;
nodes_with_no_ltree_filtered example_tree [1 ; 3 ; 5 ; 7];;
nodes_with_no_ltree_filtered example_tree [8 ; 9 ; 10];;
nodes_with_no_ltree_filtered EmptyTree [];;

(* Definition of number_of_edges *)

let rec number_of_edges t =
  match t with
  | EmptyTree -> 0
  | Node (_, l, r) ->
      let edges_here = 
        (if l <> EmptyTree then 1 else 0) + 
        (if r <> EmptyTree then 1 else 0)
      in
      edges_here + number_of_edges l + number_of_edges r;;

(* Test calls for number_of_edges *)
number_of_edges example_tree;;
number_of_edges EmptyTree;;
