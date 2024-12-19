(* Define the type of a 2-3 tree node, including Leaf *)
type 'a t
(** Abstraction Function (AF):
    - The Leaf represents the empty set.
    - TwoNode and ThreeNode structures represent a set with elements stored in a
      balanced order.

    Representation Invariant (RI):
    - Each TwoNode or ThreeNode maintains ordered elements.
    - All leaves are at the same depth.
    - Internal nodes are either 2-nodes or 3-nodes. *)

val empty : 'a t
(** [empty] is tree with no leaf nodes (children). *)

val is_empty : 'a t -> bool
(** [empty tree] is the result of checking whether a tree is empty or not. *)

val mem : 'a -> 'a t -> bool
(** [mem v tree] is the result of checking whether node/value [v] exists within
    a tree. *)

val insert : 'a -> 'a t -> 'a t
(** [insert v tree] is the resulting 2-3 tree after inserting a certain value
    [v] into 2-3 tree [tree]. *)

val to_string_int_tree : int t -> string
(** [to_string v tree] is the result of converting a tree into the string
    repsented equivalent. *)

val rep_ok : 'a t -> bool
(** [rep_ok tree] is the result of checking the representation invariant is
    satisfied. *)
