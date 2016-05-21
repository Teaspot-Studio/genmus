module Data.SymReg.Functions(
    calc
  , operands
  , plusOp
  , minusOp
  , multiplyOp
  , divideOp
  , powOp
  , negationOp
  , sqrtOp
  , lnOp
  , logOp
  , absOp
  , sinOp
  , cosOp
  ) where

import Data.Text

import Data.SymReg.AST

operands :: [Operand]
operands = [
    plusOp
  , minusOp
  , multiplyOp
  , divideOp
  , powOp
  , negationOp
  , sqrtOp
  , lnOp
  , logOp
  , absOp
  , sinOp
  , cosOp
  ]

calc :: ASTPoint -> Operand -> Double
calc args Operand{..} = operandImpl args

nan :: Double
nan = 0/0

plusOp :: Operand
plusOp = Operand{
    operandName = "+"
  , operandType = InfixOperand
  , operandArity = 2
  , operandPriority = 0 
  , operandImpl = (\args -> 
      case args of 
        [] -> nan
        x:[] -> nan
        otherwise -> sum args)
}

minusOp :: Operand
minusOp = Operand{
    operandName = "-"
  , operandType = InfixOperand
  , operandArity = 2
  , operandPriority = 0 
  , operandImpl = (\args -> 
      case args of 
        x:y:[] -> x - y
        otherwise -> nan)
  }

multiplyOp :: Operand
multiplyOp = Operand{
    operandName = "*"
  , operandType = InfixOperand
  , operandArity = 2
  , operandPriority = 1 
  , operandImpl = (\args -> 
      case args of 
        [] -> nan
        x:[] -> nan
        otherwise -> product args)
  }

divideOp :: Operand
divideOp = Operand{
    operandName = "/"
  , operandType = InfixOperand
  , operandArity = 2
  , operandPriority = 1 
  , operandImpl = (\args -> 
      case args of 
        x:y:[] -> x/y
        otherwise -> nan)
  }

powOp :: Operand
powOp = Operand{
    operandName = "^"
  , operandType = InfixOperand
  , operandArity = 2
  , operandPriority = 3 
  , operandImpl = (\args -> 
      case args of 
        x:y:[] -> x**y
        otherwise -> nan)
  }

negationOp :: Operand
negationOp = Operand {
    operandName = "-"
  , operandType = PrefixOperand
  , operandArity = 1
  , operandPriority = 3 
  , operandImpl = (\args -> 
      case args of 
        x:[] -> negate x
        otherwise -> nan)
  }

simpleUnarPrefix :: Text -> (Double -> Double) -> Operand 
simpleUnarPrefix name impl = Operand {
    operandName = name
  , operandType = PrefixOperand
  , operandArity = 1
  , operandPriority = 4 
  , operandImpl = (\args -> 
      case args of 
        x:[] -> impl x
        otherwise -> nan)
  }

simpleBinarPrefix :: Text -> (Double -> Double -> Double) -> Operand 
simpleBinarPrefix name impl = Operand {
    operandName = name
  , operandType = PrefixOperand
  , operandArity = 2
  , operandPriority = 4 
  , operandImpl = (\args -> 
      case args of 
        x:y:[] -> impl x y
        otherwise -> nan)
  }

sqrtOp :: Operand
sqrtOp = simpleUnarPrefix "sqrt" sqrt

lnOp :: Operand
lnOp = simpleUnarPrefix "ln" log

logOp :: Operand
logOp = simpleBinarPrefix "log" logBase 

absOp :: Operand
absOp = simpleUnarPrefix "abs" abs

sinOp :: Operand
sinOp = simpleUnarPrefix "sin" sin

cosOp :: Operand
cosOp = simpleUnarPrefix "cos" cos