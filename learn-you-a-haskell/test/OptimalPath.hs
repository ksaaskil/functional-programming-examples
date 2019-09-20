module OptimalPath where

import           FunctionallySolvingProblems   as F
import           Test.HUnit

expectedPath :: F.Path
expectedPath =
    [(F.B, 10), (F.C, 30), (F.A, 5), (F.C, 20), (F.B, 2), (F.B, 8), (F.C, 0)]

-- Optimal path solutions
testSolution =
    TestCase
        $ assertEqual "Optimal path should be as expected" expectedPath
        $ F.optimalPath F.heathrowToLondon

testRoadStep =
    TestCase
        $ assertEqual "Road step should work as expected for the first step"
                      ([(F.C, 30), (F.B, 10)], [(B, 10)])
        $ roadStep ([], []) (head F.heathrowToLondon)

testOptimalPath = TestList [testRoadStep, testSolution]
