{-# LANGUAGE TypeFamilies#-}
module Data.SymReg.Evolvable(
    ASTGenOpts(..)
  ) where

import Control.Monad.State
import Control.Monad.Random

import Data.SymReg.AST
import Data.SymReg.Misc
import Data.SymReg.Functions
import Data.Genetics.Class

instance Evolvable AST where
  type EvData AST = ASTData
  type GMCOptions AST = ASTGenOpts
  generate = generateAST 
  mutate = mutateAST
  crossover gopts = crossoverAST (goMaxDepth gopts)
  fitness = fitnessAST

data ExpType = ExpConst | ExpVar | ExpFunc

data ASTGenOpts = ASTGenOpts{
  goMaxDepth :: Int
, goVarCount :: Int
, goConstRange :: (Double,Double)
, goCVF :: (Int,Int,Int)
}

generateAST :: ASTGenOpts -> IO AST
generateAST gopts = evalStateT generateAST' gopts

generateAST' :: StateT ASTGenOpts IO AST
generateAST' = do
  ASTGenOpts d varCount r (c,v,f) <- get
  e <- if(d==0) 
    then if(varCount == 0)
          then return ExpConst
          else uniform [ExpConst, ExpVar]
    else if(varCount == 0)
          then fromList $ zip [ExpConst, ExpFunc] $ fromIntegral <$> [c,f]
          else fromList $ zip [ExpConst, ExpVar, ExpFunc] $ fromIntegral <$> [c,v,f]
  case e of
    ExpConst -> do
      val <- getRandomR r
      return $ Const val
    ExpVar -> do 
      var <- getRandomR (0,(varCount-1))
      return $ Variable var
    ExpFunc -> do
      op <- liftIO $ uniform operands
      let n = operandArity op
      let opts = ASTGenOpts (d-1) varCount r (c,v,f)
      asts <- liftIO $ sequence $ evalStateT generateAST' <$> replicate n opts
      return $ Function op asts

mutateAST :: ASTGenOpts -> AST -> IO AST
mutateAST ASTGenOpts{..} ast = do
  (_,bc) <- randomNode ast
  let gopts' = ASTGenOpts (goMaxDepth - (length bc)) goVarCount goConstRange goCVF  
  node <- generate gopts'
  return $ insertNodeAt node ast bc

crossoverAST :: Depth -> (AST,AST) -> IO (AST,AST)
crossoverAST maxDepth (p1,p2) = do
  ((a1,b1),(a2,b2)) <- getPersistently 5 p1 p2
  let c1 = insertNodeAt a2 p1 b1
  let c2 = insertNodeAt a1 p2 b2
  return (c1,c2)
  where 
    getPersistently :: Int -> AST -> AST -> IO (((AST, Breadcrumb),(AST, Breadcrumb)))
    getPersistently 0 p1 p2 = return $ ((p1,[]),(p2,[]))
    getPersistently n p1 p2 = do
      (a1,b1) <- randomNode p1
      (a2,b2) <- randomBounded maxDepth (calcHeight a1, length b1) p2
      if b2 == [-1]
        then getPersistently (n-1) p1 p2
        else return ((a1,b1),(a2,b2))

fitnessAST :: ASTData -> AST -> Double 
fitnessAST datum ast = (/) (fromIntegral $ length datum) $ sum $ (\(p,a) -> (a - evalAST ast p)^^2) <$> datum