import Test.Hspec

import qualified AoC1.RocketEquationSpec
import qualified AoC2.ProgramAlarmSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RocketEquation"     AoC1.RocketEquationSpec.spec
  describe "ProgramAlarm"       AoC2.ProgramAlarmSpec.spec

