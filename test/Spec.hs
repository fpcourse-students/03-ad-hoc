import SpecBlock1 qualified
import SpecBlock2 qualified
import Test.Prelude

main :: IO ()
main = testMain $ SpecBlock1.tests ++ SpecBlock2.tests
