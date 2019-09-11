import           FunctionallySolvingProblems    ( solveRPN )
import           Test.HUnit                    as HUnit

test1 = TestCase $ assertEqual "Should 1+1 equal 2" 2 (1 + 1)
testRPN1 =
    TestCase $ assertEqual "Should handle summation" 2 $ solveRPN "1 1 +"


tests = TestList
    [TestLabel "Example test 1" test1, TestLabel "Solve RPN 1" testRPN1]

main :: IO HUnit.Counts
main = HUnit.runTestTT tests
