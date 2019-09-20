module OptimalPath where

import           FunctionallySolvingProblems   as F
import           Test.HUnit

expectedPath :: F.Path
expectedPath = [(F.B, 10), (F.C, 30), (F.A, 5), (F.C, 20), (F.B, 2), (F.B, 8)]

-- Optimal path solutions
testSolution =
    TestCase
        $ assertEqual "Optimal path should be as expected" expectedPath
        $ F.optimalPath F.heathrowToLondon

testOptimalPath = TestList [testSolution]
