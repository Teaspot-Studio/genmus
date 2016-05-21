import Criterion.Main
import Data.SymReg 
import Control.DeepSeq

testTree :: IO AST
testTree = untilM ((500 <) . calcLazy) $ generate opts
  where
  opts = GeneratorOptions {
      goMaxDepth = 20
    , goVarCount = 100
    , goConstRange = (-100, 100)
    }

  untilM :: (AST -> Bool) -> IO AST -> IO AST 
  untilM f m = do 
    a <- m 
    if f a then untilM f m 
      else return a

main = do
  tree <- testTree
  tree `deepseq` defaultMain [
      bench "randomNode lazy" $ nfIO (randomNodeLazy tree)
    , bench "randomNode data" $ nfIO (randomNode tree)
    ]