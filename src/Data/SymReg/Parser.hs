module Data.SymReg.Parser(
    parseFunction
  , showFunction
  , operandsByPriority
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable as F 
import Data.Function 
import Data.List (sortBy)
import Data.Maybe 
import Data.Monoid
import Data.SymReg.AST
import Data.SymReg.Functions 
import Data.Text as T hiding (count)
import Prelude as P
import Debug.Trace 

showt :: Show a => a -> Text 
showt = pack . show

showFunction :: AST -> Text 
showFunction a = case a of 
  Const v -> showt v 
  Variable i -> "X" <> showt i 
  Function o@Operand{..} as -> case operandType of 
    InfixOperand -> T.unwords $ [
        maybe "?" (smartShow o) (as !? 0) 
      , operandName 
      , maybe "?" (smartShow o) (as !? 1)
      ]
    PrefixOperand -> 
         operandName 
      <> parentize (intercalate ", " $ showFunction <$> as)

parentize :: Text -> Text 
parentize t = "(" <> t <> ")"

-- | parentize only if needed
smartShow :: Operand -> AST -> Text
smartShow _ v@(Const _) = showFunction v
smartShow _ v@(Variable _) = showFunction v
smartShow op v@(Function oc _) = case operandType oc of 
  InfixOperand -> if op /= oc then parentize $ showFunction v
    else showFunction v
  PrefixOperand -> showFunction v

infixl 9 !?

(!?) :: [a] -> Int -> Maybe a 
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? i = xs !? (i-1)

parseFunction :: Text -> Either String AST
parseFunction = parseOnly $ termInfixes <* many' space

termInfixes :: Parser AST
termInfixes = go $ operandsByPriority infixOps 
  where
  infixOps = P.filter ((InfixOperand ==) . operandType) operands
  
  go :: [[Operand]] -> Parser AST
  go [] = termPrefixes
  go (os:oss) = do 
    a <- go oss 
    obs <- many' $ choice $ parseInfix <$> os
    return $ constructInfixTree a obs
    where
    parseInfix :: Operand -> Parser (Operand, AST)
    parseInfix o = do 
      void $ lemma $ string $ operandName o 
      b <- go oss
      return (o, b)

  constructInfixTree :: AST -> [(Operand, AST)] -> AST 
  constructInfixTree a = F.foldl' go a 
    where
      go :: AST -> (Operand, AST) -> AST 
      go acc (o, b) = Function o [acc, b]

termPrefixes :: Parser AST 
termPrefixes = go $ operandsByPriority prefixOps 
  where
  prefixOps = P.filter ((PrefixOperand ==) . operandType) operands

  go :: [[Operand]] -> Parser AST
  go [] = parens <|> literal <|> variable
  go (os:oss) = choice (parsePrefix <$> prefixOps) <|> go oss
    where
    parsePrefix o = do
      void $ lemma $ string $ operandName o 
      functionPrefix o <|> nativePrefix o

    nativePrefix :: Operand -> Parser AST
    nativePrefix o = 
      Function o <$> count (operandArity o) (go (os:oss))

    functionPrefix :: Operand -> Parser AST
    functionPrefix o = do 
      _ <- lemma $ char '('
      res <- termInfixes `sepBy` lemma (char ',')
      _ <- lemma $ char ')'
      return $ Function o res

  optionChar c = option c $ lemma $ char c

lemma :: Parser a -> Parser a 
lemma p = many' space *> p 

literal :: Parser AST 
literal = lemma $ Const <$> double

variable :: Parser AST 
variable = lemma $ Variable <$> (char 'X' *> decimal)

parens :: Parser AST 
parens = lemma (char '(') *> termInfixes <* lemma (char ')')

operandsByPriority :: [Operand] -> [[Operand]]
operandsByPriority ops = P.reverse $ F.foldl' go [] os
  where
    os = sortBy (compare `on` operandPriority) ops 
    go [] a = [[a]]
    go ass@(as@(a:_):asx) b = if operandPriority a == operandPriority b
      then (b:as):asx
      else [b]:ass