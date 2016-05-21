module Data.Music.Music(
    Note(..)
  , NoteMod(..)
  , NoteChunk(..)
  , Melody 
  , MelodyOptions(..)
  , generateMelody
  , printMelody
  ) where

import Data.Text (Text, unpack, pack)
import Control.DeepSeq
import GHC.Generics  
import Control.Monad
import Control.Monad.Random
import Data.Monoid
import Data.Functor 

import Data.Vector (Vector)
import qualified Data.Vector as V 

data Note = 
  NoteA | NoteB | NoteC | NoteD | NoteE | NoteF | NoteG | NoteP

allNotes :: [Note]
allNotes = [NoteA, NoteB, NoteC, NoteD, NoteE, NoteF, NoteG, NoteP]

data NoteMod = 
  NoteBemol | NoteDiez

data NoteChunk = NoteChunk Note (Maybe NoteMod) (Maybe Int) | NoteUp | NoteDown | NoteLength Int 

type Melody = Vector NoteChunk

data MelodyOptions = MelodyOptions{
  melodyLengthMin :: Int  
, melodyLengthMax :: Int
, melodyOctaveModRatio :: Rational
, melodyLengthModRatio :: Rational
, melodyNoteRatio :: Rational
, melodyLengthModMin :: Int 
, melodyLengthModMax :: Int
, melodyNoteModRatio :: Rational
, melodyNoteLengthRatio :: Rational
, melodyNoteLengthMin :: Int  
, melodyNoteLengthMax :: Int
}

generateMelody :: MelodyOptions -> IO Melody
generateMelody MelodyOptions{..} = do
  melodyLength <- uniform [melodyLengthMin .. melodyLengthMax]
  chunksActions <- replicateM melodyLength $  -- sequence :: Vector (IO a) -> IO (Vector a)
    fromList [
        (genNote, melodyNoteRatio)
      , (return NoteUp, melodyOctaveModRatio / 2)
      , (return NoteDown, melodyOctaveModRatio / 2)
      , (genNoteLength, melodyLengthModRatio)]
  chunks <- sequence chunksActions
  return $ V.fromList chunks
  where 
  genNote = do 
    note <- uniform allNotes
    mode <- do 
      isgen <- fromList [(False, 1 - melodyNoteModRatio), (True, melodyNoteModRatio)]
      if isgen then Just <$> uniform [NoteBemol, NoteDiez]
        else return Nothing
    lngth <- do 
      isgen <- fromList [(False, 1 - melodyNoteLengthRatio), (True, melodyNoteLengthRatio)]
      if isgen then Just <$> uniform [melodyNoteLengthMin .. melodyNoteLengthMax]
        else return Nothing
    return $ NoteChunk note mode lngth 

  genNoteLength = NoteLength <$> uniform [melodyLengthModMin .. melodyLengthModMax]

printMelody :: Melody -> String
printMelody = V.foldl' printChunk ""
  where 
  printChunk acc ch = acc ++ " " ++ case ch of 
    NoteChunk note mode lngth -> printNote note ++ maybe "" printMode mode ++ maybe "" show lngth 
    NoteUp -> ">"
    NoteDown -> "<"
    NoteLength i -> "l" <> show i

  printMode m = case m of 
    NoteBemol -> "#"
    NoteDiez -> "-"

  printNote n = case n of 
    NoteA -> "a"
    NoteB -> "b" 
    NoteC -> "c" 
    NoteD -> "d" 
    NoteE -> "e" 
    NoteF -> "f" 
    NoteG -> "g" 
    NoteP -> "p"
