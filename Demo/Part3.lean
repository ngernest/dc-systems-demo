import Plausible
import Demo.Tyche
import Demo.SliderTune

open Plausible

partial def Plausible.Gen.suchThat (g : Gen α) (p : α → Bool) : Gen α := do
  let x ← g
  if p x then pure x else g.suchThat p




def genFiveToTen : Gen Nat :=
  Gen.chooseNatLt 5 11 (by simp)

#view_distribution genFiveToTen



def genLists : Gen (List Nat) :=
  Gen.resize (λ _ => 10) <| Gen.listOf genFiveToTen

-- Plot the lengths of lists
#view_distribution genLists with_feature List.length



-- Plot the proportion of sorted lists
#view_distribution genLists
  with_feature (λ xs =>
    List.mergeSort xs == xs && xs.length > 0)

-- Plot the proportion of non-empty lists
#view_distribution genLists
  with_feature (not ∘ List.isEmpty)

-- Plot the averages of the non-empty lists
#view_distribution genLists.suchThat (not ∘ List.isEmpty)
  with_feature (λ xs => List.sum xs / List.length xs)





-- Generates a `Nat` uniformly at random between `lower` and `upper`
-- Dragging the slider in VS Code changes the distribution
-- (note how the code also changes!)
def genBetween : Gen Nat := do
  Gen.chooseNatLt [slider|lower=0] [slider|upper=4] (by simp)

#view_distribution genBetween
