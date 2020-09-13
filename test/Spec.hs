import qualified NeatTests
import qualified SnakeTests
import           Test.HUnit

main :: IO (Counts)
main
  = runTestTT $ TestList
    [ NeatTests.tests
    , SnakeTests.tests
    ]
