import           Test.HUnit
import           Problem1                       ( result )

testProblem1 :: Test
testProblem1 = TestCase $ assertEqual "Should return 2" 2 result

tests = TestList [TestLabel "Problem 1" testProblem1]

main :: IO Counts
main = runTestTT tests
