def average (xs : List Float) : Float :=
  let sum := xs.foldr (· + ·) 0
  let count := xs.length.toFloat
  sum / count

#eval average [1, 2, 3, 4]
