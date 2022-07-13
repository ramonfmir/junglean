import JungLean.Data
import JungLean.Forest

def train_labels := "test/data/iris_train.labels"
def train_features := "test/data/iris_train.features"
def train_data := loadLabeled train_labels train_features

def test_labels := "test/data/iris_test.labels"
def test_features := "test/data/iris_test.features"
def test_data := loadLabeled test_labels test_features

def my_tree := tree 5 giniRule
def my_forest := forest my_tree 100 train_data

def my_train_labels := classify my_forest train_data
def my_test_labels := classify my_forest test_data

def length_my_list (l : IO (List String)) : IO Nat := do
  return (← l).length

def accuracyLabels (labels1 : IO (List String)) (labels2 : IO (List String)) : IO Float:= do
  return accuracy (← labels1) (← labels2)

--#eval length_my_list my_train_labels
--#eval length_my_list my_test_labels
--
--#eval my_test_labels
--#eval my_train_labels

#eval accuracyLabels my_test_labels my_test_labels
#eval accuracyLabels my_test_labels (getLabels test_data)

#eval accuracyLabels my_train_labels my_train_labels
#eval accuracyLabels my_train_labels (getLabels train_data)
