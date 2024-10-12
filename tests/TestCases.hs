module Main where
import Euler
import Test.HUnit
import qualified System.Exit as Exit

test1 :: Test
test1 = TestCase (assertEqual "should return 443839" 443839 (sum [x | x <- [100 .. 354294], digitFifthPowers x]))

test2 :: Test
test2 = TestCase (assertEqual "should return 443839" 443839 (sum [x | x <- [100 .. 354294], digitFifthPowers2 x]))

test3 :: Test
test3 = TestCase (assertEqual "should return 443839" 443839 (sum [x | x <- [100 .. 354294], digitFifthPowers3 x]))

test4 :: Test
test4 = TestCase (assertEqual "should return 443839" 443839 (sum_custom $ filter digitFifthPower2 (splitInt4 [100 .. 354294])))


tests :: Test
tests = TestList [TestLabel "test1" test1,TestLabel "test2" test2,TestLabel "test3" test3,TestLabel "test4" test4]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess