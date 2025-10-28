import Test.Tasty
import Test.Tasty.HUnit
import Unit.Simple

main :: IO ()
main = do
  defaultMain $
    testGroup
      "INet graph rewriting tests"
      [simpleParse]