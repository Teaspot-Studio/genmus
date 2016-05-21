module Data.SymReg.Misc(
    randomNode
  , insertNodeAt
  , randomBounded
  , calcHeight
  , Breadcrumb
  , Height
  , Depth
  ) where

import Control.Monad.Random
import qualified Data.List as L

import Data.SymReg.AST

type Breadcrumb = [Int]
type Height = Int
type Depth = Int

calcChildren :: AST -> Int
calcChildren ast = case ast of 
  Const _     -> 0
  Variable _  -> 0
  Function _ args -> L.foldl' (\s a -> s + calcChildren a) 1 args

calcHeight :: AST -> Height
calcHeight ast = case ast of
  Const _ -> 0
  Variable _ -> 0
  Function _ args -> (+) 1 $ maximum $ calcHeight <$> args

randomNode :: AST -> IO (AST, Breadcrumb)
randomNode ast = randomWithBread' (ast,[])

randomWithBread' :: (AST, Breadcrumb) -> IO (AST, Breadcrumb)
randomWithBread' (ast,bc) = case ast of
  Const x -> return (ast,bc)
  Variable x -> return (ast,bc)
  Function op args -> do
    let chs = fromIntegral . calcChildren <$> args
    crumb <- fromList $ zip [-1,0 .. (length args - 1)] (1:chs)
    case crumb of
      -1 -> return (ast,bc)
      otherwise -> randomWithBread' (args!!(crumb),bc++[crumb])

travel :: (AST, Breadcrumb) -> AST
travel (ast,[]) = ast
travel (Function op args, b:bc) = travel (args!!(b-1),bc)

insertNodeAt :: AST -> AST -> Breadcrumb -> AST
insertNodeAt node target [] = node
insertNodeAt node (Function op args) (b:bc) = Function op newArgs
  where 
    newArgs = take b args ++ [el] ++ drop (b+1) args
    el = insertNodeAt node (args!!b) bc

randomBounded :: Depth -> (Height,Depth) -> AST -> IO (AST,Breadcrumb)
randomBounded maxDepth hd ast = 
  case nn of
    [] -> return (ast, [-1])
    x -> uniform $ nn
  where nn = niceNodes maxDepth hd [] (ast,[])

niceNodes :: Depth -> (Height,Depth) -> [(AST,Breadcrumb)] -> (AST,Breadcrumb) -> [(AST,Breadcrumb)]
niceNodes maxDepth (h,d) acc (ast,bread) =
  case ast of
    Const x -> f
    Variable x -> f
    Function op args -> (++) f $ concat $ nices <$> zipWith (\a i -> (a,bread ++ [i])) args [0 ..]  
  where
    c = checkBounds maxDepth (h,d) (calcHeight ast, length bread)
    f = if c then (ast,bread):acc else acc
    nices = niceNodes maxDepth (h,d) []

checkBounds :: Depth -> (Height,Depth) -> (Height,Depth) -> Bool
checkBounds maxDepth (h1,d1) (h2,d2) = d2 <= maxDepth - h1 && h2 <= maxDepth - d1