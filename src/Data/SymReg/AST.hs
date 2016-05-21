{-# LANGUAGE ScopedTypeVariables #-}
module Data.SymReg.AST(
    OperandType(..)
  , Operand(..)
  , AST(..)
  , evalAST
  , ASTPoint
  , ASTData
  , ASTAnswer
  ) where

import Data.Text (Text, unpack, pack)
import Control.Monad.State
import Data.Monoid
import Control.DeepSeq
import GHC.Generics

type ASTPoint = [Double]
type ASTAnswer = Double
type ASTData = [(ASTPoint,ASTAnswer)]

data OperandType = 
    InfixOperand
  | PrefixOperand
  deriving (Show, Eq, Enum, Bounded, Generic)

instance NFData OperandType 

data Operand = Operand {
  operandName :: Text
, operandType :: OperandType 
, operandArity :: Int 
, operandPriority :: Int
, operandImpl :: ASTPoint -> Double
} deriving Generic

instance NFData Operand 

showt :: Show a => a -> Text 
showt = pack . show

instance Show Operand where 
  show Operand{..} = unpack $ "(" <> operandName 
    <> "," <> showt operandType 
    <> "," <> showt operandArity 
    <> "," <> showt operandPriority 
    <> ")"

instance Eq Operand where 
  a == b = operandName a == operandName b 

data AST = 
    Const Double
  | Variable Int
  | Function Operand ![AST]
  deriving (Eq, Show, Generic)

instance NFData AST 

evalAST :: AST -> ASTPoint -> Double
evalAST ast varval = evalState (evalAST' ast) varval

evalAST' :: AST -> State ASTPoint Double
evalAST' ast = case ast of
  Const x -> return x
  Variable i -> do
    vars <- get
    return $ vars!!i
  Function op asts -> do
    let f = operandImpl op
    vars <- sequence $ evalAST' <$> asts
    return $ f vars
