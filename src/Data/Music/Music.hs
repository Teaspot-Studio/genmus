{-#LANGUAGE ScopedTypeVariables#-}

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
, melodyOctaveMin :: Int
, melodyOctaveMax :: Int
}

generateMelody :: MelodyOptions -> Int -> IO Melody
generateMelody MelodyOptions{..} os = do
  melodyLength <- uniform [melodyLengthMin .. melodyLengthMax] 
  (_, ca) <- foldM (\(o,m) _ -> do
          n <- fromList $ getNoteList o
          t <- n
          let on = case t of
                    NoteDown -> o - 1
                    NoteUp -> o + 1
                    _ -> o
          return (on, m ++ [n])
        ) (os, ([] :: [IO NoteChunk])) [1 .. melodyLength]
  chunks <- sequence ca
  return $ V.fromList chunks

  where
  getNoteList :: Int -> [(IO NoteChunk, Rational)]
  getNoteList oct = [(genNote, melodyNoteRatio), (genNoteLength, melodyLengthModRatio)]
      ++ if oct < melodyOctaveMax then [(return NoteUp, melodyOctaveModRatio / 2)] else []
      ++ if oct > melodyOctaveMin then [(return NoteDown, melodyOctaveModRatio / 2)] else []

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
