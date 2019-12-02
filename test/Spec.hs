import Test.Hspec

import qualified RocketEquationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RocketEquation"     RocketEquationSpec.spec

