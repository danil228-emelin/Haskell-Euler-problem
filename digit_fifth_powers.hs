-- Recursion with pattern matching
splitInt :: (Integral a) => a -> [a]
splitInt 0 = []
splitInt x = x `mod` 10 : splitInt (x `div` 10)

-- Recursion with pattern matching
sumAll :: (Integral a) => [a] -> a
sumAll [] = 0
sumAll (x : xs) = x ^ 5 + sumAll xs

-- composition operator with conditional expression
digitFifthPowers :: (Integral a) => a -> Bool
digitFifthPowers a = if a == (sumAll $ splitInt a) then True else False

-- List comprehension with guards
findSum :: (Integral a) => a
findSum = sum [x | x <- [100 .. 354294], digitFifthPowers x]

-- Tail recursion with pattern matching
splitIntHelper :: (Integral a) => a -> [a]
splitIntHelper 0 = []
splitIntHelper x = x `mod` 10 : splitInt (x `div` 10)

splitInt2 :: (Integral a) => a -> [a]
splitInt2 a = splitIntHelper a

-- Tail recursion with pattern matching
sumAll2Helper :: (Integral a) => [a] -> a
sumAll2Helper [] = 0
sumAll2Helper (x : xs) = x ^ 5 + sumAll xs

sumAll2 :: (Integral a) => [a] -> a
sumAll2 xs = sumAll2Helper xs

-- Function with guards
digitFifthPowers2 :: (Integral a) => a -> Bool
digitFifthPowers2 a
  | a == (sumAll2 $ splitInt2 a) = True
  | otherwise = False

-- List comprehension with guards
findSum2 :: (Integral a) => a
findSum2 = sum [x | x <- [100 .. 354294], digitFifthPowers2 x]

--------------------------------------------------------

-- Using first class function.
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

splitInt3 :: (Integral a) => a -> [a]
splitInt3 a = unfold (\x -> x == 0) (\x -> x `mod` 10) (\x -> x `div` 10) a

-- Using first class function foldr with lamba
sumAll3 :: (Integral a) => [a] -> a
sumAll3 xs = foldr (\x y -> x ^ 5 + y) 0 xs

-- Function with guards
digitFifthPowers3 :: (Integral a) => a -> Bool
digitFifthPowers3 a
  | a == (sumAll3 $ splitInt3 a) = True
  | otherwise = False

findSum3 :: (Integral a) => a
findSum3 = sum [x | x <- [100 .. 354294], digitFifthPowers3 x]

--------------------------------------------------------

-- Use standart Prelude list functions


data Number a = Number a [a]

listNewElem :: (Integral a) => a -> Number a
listNewElem a = Number a (splitInt3 a)

splitInt4 :: (Integral a) => [a] -> [Number a]
splitInt4 xs = map listNewElem xs

digitFifthPower2 :: (Integral a) => Number a -> Bool
digitFifthPower2 (Number a xs) = if a == sumAll3 xs then True else False

sum_custom :: (Integral a) => [Number a] -> a
sum_custom [] = 0
sum_custom ((Number a ys ) : xs) = a + sum_custom xs

findSum4 :: (Integral a) => a
findSum4 = sum_custom $ filter digitFifthPower2 (splitInt4 [100 .. 354294])
