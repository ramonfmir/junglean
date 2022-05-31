import Std.Data.HashMap
import JungLean.Utils

open Std

variable {α} [BEq α] [Hashable α]

def giniImpur (l : List α) : Float :=
    let len := l.length
    let update (tbl : HashMap α Int) i :=
      if tbl.contains i then tbl.insert i (tbl.find! i + 1)
      else tbl.insert i 1
    let tbl := List.foldl (fun tbl i => update tbl i) HashMap.empty l
    let update :=
      fun s _ x => s + Float.pow ((Float.ofInt x) / (Float.ofNat len)) 2
    1 - HashMap.fold update 0 tbl

def splitImpur (impur : List α -> Float) (x_labels : List (Float × α)) (thr : Float) : Float :=
  let append left_right x_l :=
      let (left, right) := left_right
      let (x, l) := x_l
      if x < thr then (l :: left, right) else (left, l :: right)
  let (left, right) := List.foldl append ([], []) x_labels
  ((impur left) + (impur right)) / 2.0

def bestSplit (impur : List α -> Float) (x : List Float) (labels : List α) : Float × Float :=
    let x_l := List.zip x labels
    let x_l := List.sort (fun (a, _) (b, _) => a ≤ b) x_l
    let rec loop remaining_x_l best_thr best_impur :=
        match remaining_x_l with
        | [] => (best_thr, best_impur)
        | [_] => (best_thr, best_impur)
        | (x1, l1) :: (x2, l2) :: t =>
            let new_thr := (x1 + x2) / 2
            let new_impur := splitImpur impur x_l new_thr
            if new_impur < best_impur then
                loop ((x2, l2) :: t) new_thr new_impur
            else
                loop ((x2, l2) :: t) best_thr best_impur
    loop x_l 0 1

def bestGiniThr : List Float → List α → Float × Float := bestSplit giniImpur
