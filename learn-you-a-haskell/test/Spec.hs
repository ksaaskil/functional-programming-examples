import           Test.HUnit                    as HUnit

test1 = TestCase $ assertEqual "Should 1+1 equal 2" 2 (1 + 1)

tests = TestList [TestLabel "Example test 1" test1]

main :: IO HUnit.Counts
main = HUnit.runTestTT tests
