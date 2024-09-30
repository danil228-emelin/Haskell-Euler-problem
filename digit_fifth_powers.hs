-- Recursion with pattern matching
splitInt:: Integral a => a-> [a]
splitInt 0 = []
splitInt x = x `mod` 10 : splitInt (x `div` 10)

-- Recursion with pattern matching
sumAll :: Integral a => [a] -> a
sumAll [] = 0
sumAll (x:xs) = x^5 + sumAll xs

-- composition operator with conditional expression
digitFifthPowers :: Integral a => a -> Bool
digitFifthPowers a = if a == (sumAll $ splitInt a) then True else False

-- List comprehension with guards
findSum :: Integral a => a
findSum = sum [x| x <- [2..354294], digitFifthPowers x]