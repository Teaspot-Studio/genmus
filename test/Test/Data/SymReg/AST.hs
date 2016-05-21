{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Data.SymReg.AST(

  ) where

import Data.SymReg.AST
import Data.SymReg.Functions 

import Test.QuickCheck

instance Arbitrary AST where
  arbitrary = frequency [
      (2, literal)
    , (2, var)
    , (1, function)
    ]
    where 
    literal = do
      v <- arbitrary
      return $ Const $ getNonNegative v 
    var = do
      v <- arbitrary
      return $ Variable $ getNonNegative v 
    function = do
      op <- elements operands
      args <- vectorOf (operandArity op) arbitrary 
      return $ rightTreeToLeft $ Function op args

-- | Performs transformation fo AST in such manner:
-- Was: Function opa [a, Function opb [b, c]]
-- Now: Function opa [Function opb [a, b], c]
rightTreeToLeft :: AST -> AST 
rightTreeToLeft ast@(Const _) = ast
rightTreeToLeft ast@(Variable _) = ast
rightTreeToLeft ast@(Function opa args)
  | operandType opa /= InfixOperand = Function opa $ rightTreeToLeft <$> args 
  | otherwise = case args of 
    [a, Function opb [b, c]] | operandArity opa == operandArity opb -> 
      rightTreeToLeft $ Function opa [Function opb [rightTreeToLeft a, rightTreeToLeft b], rightTreeToLeft c]
    _ -> Function opa $ rightTreeToLeft <$> args  


