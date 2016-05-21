module Data.Genetics.Core where

import Control.Monad
import Control.Monad.Random
import Data.Function
import Data.Ord
import qualified Data.List as L

import Data.Genetics.Class

type Population a = [a]

data EvOptions a = EvOptions{
  ePopSize :: Int
, eMaxGen :: Int
, eMutaRate :: Double
, eElites :: Double
, eTarget :: Double
, eGMC :: GMCOptions a
}


randomPopulation :: Evolvable a => EvOptions a -> IO (Population a)
randomPopulation EvOptions{..} = replicateM ePopSize $ generate eGMC 

evolve :: Evolvable a => 
  EvOptions a -> EvData a -> IO (Population a)
evolve opts datum = do
  pop <- randomPopulation opts
  evolve' 0 opts datum pop

evolve' :: Evolvable a => 
  Int -> EvOptions a -> EvData a -> Population a -> IO (Population a)
evolve' n evo@(EvOptions{..}) datum pop = 
  if n >= eMaxGen 
    then return pop
    else do
      next <- oneStep evo datum pop
      if snd (getBestFit datum next) >= eTarget
        then return next
        else evolve' (n+1) evo datum pop

stepEvolution :: Evolvable a => 
  Int -> EvOptions a -> EvData a -> Population a -> IO [(a, Double)]
stepEvolution _ _ _ [] = return []
stepEvolution 0 _ datum pop = return $ map (\(a,f) -> if(isNaN f) then (a,0) else (a,f)) $ indiFit datum <$> pop
stepEvolution n evo datum pop = do
  next <- oneStep evo datum pop
  stepEvolution (n-1) evo datum next 

oneStep :: Evolvable a => 
  EvOptions a -> EvData a -> Population a -> IO (Population a)
oneStep _ _ [] = return []
oneStep EvOptions{..} datum pop =
  if(length matingPool < 2) 
    then return []
    else do
      kids <- replicateM n $ do
        p1 <- fromList matingPool
        p2 <- fromList matingPool
        (c,d) <- crossover eGMC (p1,p2)
        c' <- mayMutate c
        d' <- mayMutate d
        sequence [c',d']
      return $ fst $ L.splitAt ePopSize $ (++) elites $ concat kids
  where
    n = ePopSize `div` 2 + ePopSize `mod` 2 
    e = (*) eElites $ fromIntegral $ length matingPool
    matingPool = foldl (\acc i -> let f = fitness datum i in if(isNaN f) then (i, toRational 0):acc else (i, toRational f):acc) [] pop
    elites = fst $ L.splitAt (ceiling e) $ map fst $ L.sortBy (compare `on` (Down . snd)) $ matingPool
    mr = toRational eMutaRate
    mayMutate e = fromList [(mutate eGMC e, mr), (return e, 1 - mr)]

indiFit :: Evolvable a => EvData a -> a -> (a,Double)
indiFit datum indi = (indi, fitness datum indi)

getBestFit :: Evolvable a => EvData a -> Population a -> (a,Double)
getBestFit datum pop = head $ L.sortBy (compare `on` snd) $ indiFit datum <$> pop