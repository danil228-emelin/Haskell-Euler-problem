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
--------------------------------------------------------

-- Tail recursion with pattern matching
splitIntHelper:: Integral a => a-> [a]
splitIntHelper 0 = []
splitIntHelper x = x `mod` 10 : splitInt (x `div` 10)

splitInt2 :: Integral a => a-> [a]
splitInt2 a = splitIntHelper a

-- Tail recursion with pattern matching
sumAll2Helper:: Integral a =>[a] -> a
sumAll2Helper [] = 0
sumAll2Helper (x:xs) = x^5 + sumAll xs

sumAll2:: Integral a =>[a] -> a
sumAll2 xs = sumAll2Helper xs

-- Function with guards
digitFifthPowers2 :: Integral a => a -> Bool
digitFifthPowers2 a 
                    |a == (sumAll2 $ splitInt2 a) = True
                    | otherwise = False    

-- List comprehension with guards
findSum2 :: Integral a => a
findSum2 = sum [x| x <- [2..354294], digitFifthPowers2 x]                    