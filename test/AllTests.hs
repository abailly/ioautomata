import           ATMTest
import           Control.Monad
import           Test.HUnit    hiding (Path)


main :: IO ()
main = void $ runTestTT allTests

allTests :: Test
allTests = TestList [ atmTransitionsUpdateState
                    , testATMMachineValidator
                    , testATMStateT
                    ]
