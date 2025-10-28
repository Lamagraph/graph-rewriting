module Unit.Simple where

import Common.Term (parse)
import Test.Tasty
import Test.Tasty.HUnit

simpleParse :: TestTree
simpleParse =
  testCase "parse simple combinator" $
    show (parse "SKI") @?= "S K I"