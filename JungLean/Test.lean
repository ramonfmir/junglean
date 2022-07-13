import JungLean.Data
import JungLean.Forest

def train_labels := "test/data/iris.labels"
def train_features := "test/data/iris.features"
def train_data := loadLabeled train_labels train_features
#check train_data

def my_tree := tree 10 giniRule
def my_forest := forest my_tree 10 train_data
#check my_forest

def my_trained_tree := tree 10 giniRule train_data
#check my_trained_tree

def my_labels := classify my_forest train_data
#check my_labels

def length_my_list (l : IO (List (IO String))) : IO Nat := do
  let ll := (← l).length
  return ll

def print_my_list (l : IO (List (IO String))) : IO Unit := do
  for i in (← l) do
    IO.println (← i)
  return

#eval length_my_list my_labels
#eval print_my_list my_labels
