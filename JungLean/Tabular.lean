import JungLean.Utils
import JungLean.Impurity

def Labels := Array String
def Example := Array Float
def Features := Array (Array Float)
def Indices := List Nat

structure Examples where
  indices : Indices
  features : Features
  labels : Labels

def loadFeatures (path : String) : IO (Array (Array Float)) := do
  let lines ← readLines path
  let readLine l := (l.splitOn ",").map floatOfString
  let lines := List.map readLine lines
  return Array.mk (List.map Array.mk lines)

def loadLabels (path : String) : IO (Array String) := do
  let lines ← readLines path
  return Array.mk lines

def printExample (x : Examples) (i : Nat) : IO Unit := do
  let features := x.features[i]
  let label := x.labels[i]
  for f in features do IO.print f; IO.print " "
  IO.println label

def nFeatures (x : Examples) : Nat :=
  Array.size x.features[0]

def colToList (n : Nat) (x : Examples) :=
  List.map (fun i => x.features[i][n]) x.indices

--TODO labels as option
def labels (x : Examples) : List String :=
  List.map (fun i => x.labels[i]) x.indices

def randomThr (l : List Float) : IO Float := do
  let min := minList l
  let l_no_min := List.filter (fun i => i > min) l
  if (List.length l_no_min) = 0 then return l.get! 0
  else return l_no_min.get! (← IO.rand 0 (l_no_min.length - 1))

def randomRule (examples : Examples) : IO (Example → Bool) := do
  let n ← IO.rand 0 (nFeatures examples - 1)
  let col := colToList n examples
  let thr ← randomThr col
  return (fun e => e[n] < thr)

def giniRule (examples : Examples) : IO (Example → Bool) := do
    let n := nFeatures examples
    let m := n |> Float.ofInt |> Float.sqrt |> Float.toInt |> Int.toNat
    let random_cols ← sample (List.range n) m
    let labels := labels examples
    let rec loop rc thrs_impurs :=
        match rc with
        | [] => List.reverse thrs_impurs
        | h :: t =>
            let col := colToList h examples
            let thr_impur := bestGiniThr col labels
            loop t (thr_impur :: thrs_impurs)
    let thrs_impurs := loop random_cols []
    let cols_impurs := List.zip random_cols thrs_impurs
    let im := fun (_, _, i) => i
    let cols_impurs_sorted :=
        List.sort (fun a b => (im a) < (im b)) cols_impurs
    let (best_col, best_thr) :=
        match cols_impurs_sorted with
        | []             => panic! "Empty list"
        | (c, t, _) :: _ => (c, t)
    return (fun e => e[best_col] < best_thr)
