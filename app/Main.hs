module Main where

import Data.SymReg
import Data.Genetics
import Data.Music.Music

opts :: MelodyOptions
opts = MelodyOptions {
    melodyLengthMin = 50
  , melodyLengthMax = 200
  , melodyOctaveModRatio = 0.1
  , melodyLengthModRatio = 0.1
  , melodyNoteRatio = 0.8
  , melodyLengthModMin = 1
  , melodyLengthModMax = 16
  , melodyNoteModRatio = 0.2
  , melodyNoteLengthRatio = 0.3
  , melodyNoteLengthMin = 1
  , melodyNoteLengthMax = 16
  }

main :: IO ()
main = do
  melody <- generateMelody opts
  let ms = printMelody melody 
  putStrLn ms 
