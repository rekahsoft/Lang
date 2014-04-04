square = (x) -> x * x
cube = (x) -> x * x * x

map = (f, xs) -> f x for x in xs

map square, [1..10]

foldr = (f, i, xs) ->
  result = i
  for x in xs
    result = f x, result
  result

foldr ((a, b) -> a + b), 0, [1..10]

factorial = (n) -> foldr ((a, b) -> a * b), 1, [1..n]

factorial n for n in [1..10]
