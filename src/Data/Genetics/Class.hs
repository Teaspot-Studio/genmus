{-# LANGUAGE TypeFamilies#-}

module Data.Genetics.Class where

class Evolvable a where
  type EvData a :: *
  type GMCOptions a :: *
  generate :: GMCOptions a -> IO a
  mutate :: GMCOptions a -> a -> IO a
  crossover :: GMCOptions a -> (a,a) -> IO (a,a)
  fitness :: EvData a -> a -> Double