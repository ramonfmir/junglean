/- Ported from:
https://github.com/BartoszPiotrowski/random-forest/blob/main/src/utils.ml
-/

import Init.Data.Random

section List

variable {α} [Inhabited α]

def minList [LE α] [DecidableRel (@LE.le α _)] : List α → α
  | []         => panic! "Empty list"
  | l@(h :: _) => l.foldl min h

def maxList [LT α] [DecidableRel (@LT.lt α _)] : List α → α
  | []         => panic! "Empty list"
  | l@(h :: _) => l.foldl max h

def accuracy [DecidableEq α] (l₁ l₂ : List α) : Float :=
  assert! l₁.length = l₂.length
  let pairs := List.zip l₁ l₂
  let correct := pairs.filter (fun (x, y) => x = y)
  (Float.ofNat correct.length) / (Float.ofNat pairs.length)

def arraySubset (x : List α) (inds : List Nat) : Array α :=
  Array.mk $ inds.map (fun i => x.get! i)

def sampleWithReplace (l : List α) (n : Nat) : IO (List α) :=
  let a := Array.mk l
  let rec loop i r :=
    match i with
    | 0 => return r
    | k + 1 => do loop k $ a.get! (← IO.rand 0 (a.size - 1)) :: r
  loop n []

def sample (l : List α) (n : Nat) : IO (List α) :=
  if l.length < n then panic! "List shorter than n" else do
    let mut a := Array.mk l
    for i in List.range n do
      let j ← IO.rand 0 (a.size - i - 1)
      let e := a.get! (i + j)
      a := a.set! (i + j) (a.get! i)
      a := a.set! i e
    return (a.extract 0 n).data

def chooseRandom (l : List α) : IO α := do
  return l.get! (← IO.rand 0 (l.length - 1))

def insert (compare : α → α → Bool) (x : α) (l : List α) : (List α) :=
  match l with
  | [] => [x]
  | h :: t => if compare x h then x :: h :: t else h :: insert compare x t

-- TODO better sorting algo
def sort (compare : α → α → Bool) (l : List α) : (List α) :=
  let rec loop : List α → List α → List α
    | acc, [] => acc
    | acc, h :: t => loop (insert compare h acc) t
  loop [] l

end List

def readLines (path : String) : IO (List String) := do
  let handle ← IO.FS.Handle.mk path IO.FS.Mode.read
  let content ← handle.readToEnd
  return (content.splitOn "\n")

def time (f : α → β) (x : α) : IO β := do
  timeit "Execution time: " (return f x)
