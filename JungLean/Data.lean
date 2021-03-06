import JungLean.Tabular

def loadLabeled (labels : String) (features : String) : IO Examples := do
  let features ← loadFeatures features
  let labels ← loadLabels labels
  let n := Array.size features
  let indices := List.range n
  return {indices := indices, features := features, labels := labels}

def getLabels (examples : IO Examples) : IO (List String) := do
  let labels := (← examples).labels
  let indices := (← examples).indices
  let indices := List.sort (fun a b => a < b) indices
  let labels := List.map (fun i => labels.get! i) indices
  return labels

def indices (e : Examples) : List Nat := e.indices

def print (e : Examples) : IO Unit := do
  let inds := indices e
  for i in inds do printExample e i

def isEmpty (e : Examples) : Bool :=
  e.indices.length = 0

def firstLabel (e : Examples) :=
  match e.indices with
  | []     => panic! "Empty examples"
  | h :: t => e.labels[h]

def randomLabel (e : Examples) : IO String:= do
    let i ← chooseRandom e.indices
    return e.labels[i]

def randomSubset (e : Examples) : IO Examples := do
    let random_indices_dup ← sampleWithReplace e.indices (e.indices.length)
    let random_indices := dedup random_indices_dup
    return {e with indices := random_indices}

def uniformLabels (e : Examples) : Bool :=
  let rec uniform inds :=
    match inds with
    | []            => True
    | [_]           => True
    | h1 :: h2 :: t =>
      if e.labels[h1] = e.labels[h2] then uniform (h2 :: t) else False
  uniform e.indices

def split (rule : Array Float → Bool) (e : Examples) : (Examples × Examples) :=
  let rec loop inds_l inds_r l :=
    match l with
    | [] => (inds_l, inds_r)
    | h :: t =>
      match (rule e.features[h]) with
      | false => loop inds_l (h :: inds_r) t
      | true  => loop (h :: inds_l) inds_r t
  let (inds_l, inds_r) := loop [] [] e.indices
  ({e with indices := inds_l}, {e with indices := inds_r})
