import           Test.HUnit
import           Problem1                       ( result
                                                , resultForBelow
                                                )

testProblem1ResultForBelow = TestCase
    $ assertEqual "Should return 23 for under 10" 23 (resultForBelow 10)

testProblem1Result =
    TestCase $ assertEqual "Should return 233168" 233168 result

tests = TestList
    [ TestLabel "Problem 1"        testProblem1Result
    , TestLabel "Problem 1 helper" testProblem1ResultForBelow
    ]

main :: IO Counts
main = runTestTT tests
