import qualified BreedingTests
import qualified MutationTests
import qualified SnakeTests
import           Test.HUnit

main :: IO (Counts)
main
  = runTestTT $ TestList
    [ BreedingTests.tests
    , MutationTests.tests
    , SnakeTests.tests
    ]
