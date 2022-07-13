import JungLean.Data
import JungLean.Tree
import Std.Data.HashMap

def forest (tree : IO Examples → IO Tree) (n : Nat) (examples : IO Examples) : IO (List Tree) := do
  let initseg := List.range n
  let examples ← examples
  let forest := List.map (fun i => tree (randomSubset examples)) initseg
  evalList forest

open Std

variable {α} [BEq α] [Hashable α] [Inhabited α]

def vote (votes : List String) : IO String := do
  let update (tbl : HashMap String Int) cl :=
    if tbl.contains cl then tbl.insert cl (tbl.find! cl + 1)
    else tbl.insert cl 1
  let tbl := List.foldl (fun tbl i => update tbl i) HashMap.empty votes
  let update_max_cl maxs cl v :=
    let (max_cls, max_v) := maxs
    if (v > max_v) then ([cl], v)
    else (if v = max_v then (cl :: max_cls, v) else (max_cls, max_v))
  let max_freq_cls := HashMap.fold update_max_cl ([], 1) tbl
  match max_freq_cls with
  | ([], _) => panic! "At least one class needs to have maximal frequency."
  | ([x], _) => return x
  | (l, _) => return (← chooseRandom l)

def classify (forest : IO (List Tree)) (examples : IO Examples) : IO (List String) := do
  let examples ← examples
  let forest ← forest
  let votes := List.map (Tree.classify examples) forest
  let votes ← evalList votes
  let inds := indices examples
  let voted i := (vote (List.map (fun x => List.get! x i) votes))
  evalList (List.map voted inds)
