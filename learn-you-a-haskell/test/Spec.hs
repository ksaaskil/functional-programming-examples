import           FunctionallySolvingProblems    ( solveRPN )
import           Test.HUnit                    as HUnit
import           RPN                            ( testRPN )

exampleTest = TestCase $ assertEqual "Should sum 1+1 to 2" 2 (1 + 1)

{- tests = TestList
    [TestLabel "Example test 1" test1, TestLabel "Solve RPN 1" testRPN1] -}

main :: IO HUnit.Counts
main = HUnit.runTestTT $ TestList [exampleTest, testRPN]
