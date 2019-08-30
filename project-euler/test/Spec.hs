import           Test.HUnit
import           Problem1                       ( result
                                                , resultForBelow
                                                )

testProblem1ResultForBelow = TestCase
    $ assertEqual "Should return 15 for under 20" 15 (resultForBelow 20)

testProblem1Result = TestCase $ assertEqual "Should return 33165" 33165 result

tests = TestList
    [ TestLabel "Problem 1"        testProblem1Result
    , TestLabel "Problem 1 helper" testProblem1ResultForBelow
    ]

main :: IO Counts
main = runTestTT tests
