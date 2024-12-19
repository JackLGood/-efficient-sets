type 'a t =
  | Leaf
  | TwoNode of 'a * 'a t * 'a t
  | ThreeNode of 'a * 'a * 'a t * 'a t * 'a t

let empty = Leaf

let rec is_empty tree =
  match tree with
  | Leaf -> true
  | _ -> false

let rec mem v tree =
  match tree with
  | Leaf -> false
  | TwoNode (v', lt, rt) ->
      if v < v' then mem v lt else if v > v' then mem v rt else true
  | ThreeNode (v1, v2, lt, md, rt) ->
      if v < v1 then mem v lt
      else if v = v1 then true
      else if v < v2 then mem v md
      else if v = v2 then true
      else mem v rt

(** [insert_helper v tree] is the resulting 2-3 tree after inserting a certain
    value [v] into 2-3 tree [tree] buttttt this one has the boolean if it grew
    or not, not ideal for testing. *)
and insert_helper v tree =
  match tree with
  | Leaf -> (TwoNode (v, Leaf, Leaf), true)
  | TwoNode (v', lt, rt) ->
      if v = v' then (TwoNode (v', lt, rt), false)
      else if v < v' then
        let new_lt, grow = insert_helper v lt in
        if grow then
          match new_lt with
          | TwoNode (v'', l_of_lt, l_of_rt) ->
              (ThreeNode (v'', v', l_of_lt, l_of_rt, rt), false)
          | _ ->
              failwith
                "Unexpected result after growth from left child (prob not \
                 gonna get here)"
        else (TwoNode (v', new_lt, rt), false)
      else
        let new_rt, grow = insert_helper v rt in
        if grow then
          match new_rt with
          | TwoNode (v'', lrt, r_of_rt) ->
              (* Merge with right leaf to form a 3-node *)
              (ThreeNode (v', v'', lt, lrt, r_of_rt), false)
          | _ -> failwith "Bad result after growth from right child"
        else (TwoNode (v', lt, new_rt), false)
  | ThreeNode (v1, v2, lt, md, rt) ->
      if v = v1 || v = v2 then (ThreeNode (v1, v2, lt, md, rt), false)
      else if v < v1 then
        let new_lt, grow = insert_helper v lt in
        if grow then
          match new_lt with
          | TwoNode (v'', l_of_lt, l_of_rt) ->
              (* Split parent and move up middle value *)
              let left = TwoNode (v'', l_of_lt, l_of_rt) in
              let right = TwoNode (v2, md, rt) in
              (TwoNode (v1, left, right), true)
          | _ -> failwith "Weird struct after growth from left child"
        else (ThreeNode (v1, v2, new_lt, md, rt), false)
      else if v < v2 then
        let new_md, grow = insert_helper v md in
        if grow then
          match new_md with
          | TwoNode (v'', l_of_md, r_of_md) ->
              (* Split parent and move upp middle value *)
              let left = TwoNode (v1, lt, l_of_md) in
              let right = TwoNode (v2, r_of_md, rt) in
              (TwoNode (v'', left, right), true)
          | _ -> failwith "Someting failed after growth from middle child"
        else (ThreeNode (v1, v2, lt, new_md, rt), false)
      else
        let new_rt, grow = insert_helper v rt in
        if grow then
          match new_rt with
          | TwoNode (v'', l_of_rt, r_of_rt) ->
              (* Split parent and move up middle value *)
              let left = TwoNode (v1, lt, md) in
              let right = TwoNode (v'', l_of_rt, r_of_rt) in
              (TwoNode (v2, left, right), true)
          | _ -> failwith "Fault after growth from right child"
        else (ThreeNode (v1, v2, lt, md, new_rt), false)

let rec insert v tree =
  let new_tree, _ = insert_helper v tree in
  new_tree

[@@@coverage off]

let rec to_string_int_tree tree =
  match tree with
  | Leaf -> "Leaf"
  | TwoNode (v, left, right) ->
      "(" ^ string_of_int v ^ ", " ^ to_string_int_tree left ^ ", "
      ^ to_string_int_tree right ^ ")"
  | ThreeNode (v1, v2, left1, middle, right2) ->
      "(" ^ string_of_int v1 ^ ", " ^ string_of_int v2 ^ ", "
      ^ to_string_int_tree left1 ^ ", " ^ to_string_int_tree middle ^ ", "
      ^ to_string_int_tree right2 ^ ")"

let rec rep_ok tree =
  match tree with
  | Leaf -> true
  | TwoNode (_, lt, rt) -> true
  | ThreeNode (_, _, lt, md, rt) -> true

[@@@coverage on]
