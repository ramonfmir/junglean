import Std.Data.HashMap
import JungLean.Utils
import JungLean.Data
import JungLean.Tabular

open Std

--variable {α} [Inhabited α] [BEq α]
def SplitRule := Examples → Examples × Examples

inductive Tree where
  | node : SplitRule → Tree → Tree → Tree
  | leaf : IO String → Tree

def tree max_depth (rule : Examples → Array Float → Bool) examples :=
  let rec loop examples depth :=
    match depth, (uniformLabels examples) with
    | _, true  => Tree.leaf (randomLabel examples)
    | 0, _     => Tree.leaf (randomLabel examples)
    | d + 1, _ =>
      let split := split (rule examples)
      let (examples_l, examples_r) := split examples
      if isEmpty examples_l || isEmpty examples_r
      then Tree.leaf (randomLabel examples)
      else Tree.node split (loop examples_l d) (loop examples_r d)
  loop examples max_depth

def classify (examples : Examples) (tree : Tree) :=
  let rec loop tree examples :=
    match tree with
    | Tree.leaf c =>
      List.map (fun i => (i, c)) (indices examples)
    | Tree.node split_rule tree_l tree_r =>
      let (examples_l, examples_r) := split_rule examples
      (loop tree_l examples_l) ++ (loop tree_r examples_r)
  let inds_labels := loop tree examples
  let inds := indices examples
  let tbl := inds_labels.foldl (fun t (a, b) => t.insert a b) HashMap.empty
  List.map (fun i => (tbl.find! i)) inds
