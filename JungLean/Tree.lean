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

def tree (max_depth : Nat) (rule : Examples → IO (Array Float → Bool)) (examples : IO Examples) : IO Tree := do
  let examples ← examples
  let rec loop examples depth := do
    match depth, (uniformLabels examples) with
    | _, true  => return (Tree.leaf (randomLabel examples))
    | 0, _     => return Tree.leaf (randomLabel examples)
    | d + 1, _ =>
      let rule := ← (rule examples)
      let (examples_l, examples_r) := split rule examples
      if isEmpty examples_l || isEmpty examples_r
      then return Tree.leaf (randomLabel examples)
      else return Tree.node (split rule) (← loop examples_l d) (← loop examples_r d)
  loop examples max_depth

def Tree.classify (examples : Examples) (tree : Tree) : IO (List String) := do
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
  let l := List.map (fun i => (tbl.find! i)) inds
  evalList l
