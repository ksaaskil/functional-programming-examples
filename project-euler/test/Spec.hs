import           Test.HUnit
import           Problem1                       ( result )

foo :: Int -> Int
foo = (+ 1)

testFoo :: Test
testFoo = TestCase $ assertEqual "Should return 1" 1 result

tests = TestList [TestLabel "Problem 1" testFoo]

main :: IO Counts
main = runTestTT tests
