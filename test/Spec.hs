import qualified NeatTests
import           Test.HUnit

main :: IO (Counts)
main
  = runTestTT $ TestList
    [ NeatTests.tests
    ]
