module RPN where

import           FunctionallySolvingProblems
import           Test.HUnit

exampleTest = TestCase $ assertEqual "Should sum 1+1 to 2" 2 (1 + 1)

-- RPN tests
testRPN1 =
    TestCase $ assertEqual "RPN should sum '1 1 +' to 2" 2 $ solveRPN "1 1 +"

testRPN2 =
    "testRPN2" ~: assertEqual "RPN should sum '1 2 +' to 3" 3 $ solveRPN "1 2 +"

testRPN = TestList [testRPN1, testRPN2]
