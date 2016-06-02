module Main where

import Data.SymReg
import Data.Genetics
import Data.Music.Music
import MML

opts :: MelodyOptions
opts = MelodyOptions {
    melodyLengthMin = 50
  , melodyLengthMax = 200
  , melodyOctaveModRatio = 0.1
  , melodyLengthModRatio = 0.1
  , melodyNoteRatio = 0.8
  , melodyLengthModMin = 1
  , melodyLengthModMax = 9
  , melodyNoteModRatio = 0.2
  , melodyNoteLengthRatio = 0.3
  , melodyNoteLengthMin = 1
  , melodyNoteLengthMax = 9
  , melodyOctaveMin = 1
  , melodyOctaveMax = 4
  , melodyMutateRatio = 0.3
  , melodyBlockLength = 8
}

main :: IO ()
main = do
  melody <- generateMelody opts 2
  melody2 <- generateMelody opts 2
  mutatedMelody <- mutateMelody opts melody
  crossoveredMelodies <- crossoverMelodies opts (melody, melody2)
  let ms = printMelody melody
  let ms2 = printMelody melody2
  let newMs = printMelody $ fst crossoveredMelodies
  let newMs2 = printMelody $ snd crossoveredMelodies
  let mms =  printMelody mutatedMelody
  putStrLn ms 
  putStrLn "---------------------------------------------------"
  putStrLn ms2
  putStrLn "==================================================="
  putStrLn newMs 
  putStrLn "---------------------------------------------------"
  putStrLn newMs2
  --putStrLn mms

  saveMelody melody 2 "new.midi"
  print "saved"
