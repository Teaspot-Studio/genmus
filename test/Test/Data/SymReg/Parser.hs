module Test.Data.SymReg.Parser(
    testModule
  ) where

import Data.SymReg.AST
import Data.SymReg.Functions
import Data.SymReg.Parser
import Data.Text 
import Data.Monoid

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck.Modifiers

import Test.Data.SymReg.AST()

testModule :: TF.Test
testModule = TF.testGroup "Parser" [
    TF.testGroup "Showing" [
      testProperty "consts" prop_showConst
    , testCase "plus1" $ assertEqual "" "1.0 + 2.0" $ showFunction $ Function plusOp [Const 1, Const 2]
    , testCase "plus2" $ assertEqual "" "X1 + sin(1.0, 2.0)" $ showFunction $ Function plusOp [Variable 1, Function sinOp [Const 1, Const 2]]
    , testCase "plus3" $ assertEqual "" "1.0 + 2.0 + 3.0" $ showFunction $ Function plusOp [Const 1, Function plusOp [Const 2, Const 3]]
    , testCase "plus4" $ assertEqual "" "1.0 + (2.0 - 3.0)" $ showFunction $ Function plusOp [Const 1, Function minusOp [Const 2, Const 3]]
    ]
  , TF.testGroup "Parsing" [
      testCase "const" $ assertEqual "" (Right $ Const 1) (parseFunction "1.0")
    , testCase "simple" $ assertEqual "" (Right $ Function plusOp [Const 1, Const 2]) (parseFunction "1.0 + 2.0")
    , testCase "simple" $ assertEqual "" (Right $ Function plusOp [Function plusOp [Const 1, Const 2], Const 3]) (parseFunction "1.0 + 2.0 + 3.0")
    , testCase "same priority" $ assertEqual "" (Right $ Function minusOp [Function plusOp [Const 1, Const 2], Const 3]) (parseFunction "1.0 + 2.0 - 3.0")
    , testCase "diff priority" $ assertEqual "" (Right $ Function plusOp [Const 1, Function multiplyOp [Const 2, Const 3]]) (parseFunction "1.0 + 2.0 * 3.0")
    , testCase "diff priority" $ assertEqual "" (Right $ Function plusOp [Function multiplyOp [Const 1, Const 2], Const 3]) (parseFunction "1.0 * 2.0 + 3.0")
    , testCase "function1" $ assertEqual "sin 1.0" (Right $ Function sinOp [Const 1]) (parseFunction "sin 1.0")
    , testCase "function1" $ assertEqual "sin(1.0)" (Right $ Function sinOp [Const 1]) (parseFunction "sin(1.0)")
    , testCase "function2" $ assertEqual "log 2 10" (Right $ Function logOp [Const 2, Const 10]) (parseFunction "log 2 10")
    , testCase "function2" $ assertEqual "log(2, 10)" (Right $ Function logOp [Const 2, Const 10]) (parseFunction "log(2, 10)")
    , testProperty "var" prop_parseVariable
    , testCase "negation" $ assertEqual "-(1)" (Right $ Function negationOp [Const 1]) (parseFunction "-(1)")
    , testCase "negation" $ assertEqual "-1" (Right $ Function negationOp [Const 1]) (parseFunction "-1")
    , testCase "negation" $ assertEqual "-1+1" (Right $ Function plusOp [Function negationOp [Const 1], Const 1]) (parseFunction "-1+1")
    , testCase "negation" $ assertEqual "-1+(1)" (Right $ Function plusOp [Function negationOp [Const 1], Const 1]) (parseFunction "-1+(1)")
    , testCase "negation" $ assertEqual "-1+(-1)" (Right $ Function plusOp [Function negationOp [Const 1], Function negationOp [Const 1]]) (parseFunction "-1+(-1)")
    , testCase "negation" $ assertEqual "-1--1" (Right $ Function minusOp [Function negationOp [Const 1], Function negationOp [Const 1]]) (parseFunction "-1--1") 
    ]
  , TF.testGroup "Showing-Parsing" [
      testProperty "gen-parse" prop_showParse
    ]
  ]


showt :: Show a => a -> Text 
showt = pack . show

prop_showConst :: Double -> Bool
prop_showConst v = showFunction (Const v) == showt v 

prop_parseVariable :: NonNegative Int -> Bool 
prop_parseVariable (NonNegative i) = (Right $ Variable i) == parseFunction ("X" <> showt i)

prop_showParse :: AST -> Bool 
prop_showParse ast = parseFunction (showFunction ast) == Right ast