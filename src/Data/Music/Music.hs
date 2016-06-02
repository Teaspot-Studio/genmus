{-#LANGUAGE ScopedTypeVariables#-}

module Data.Music.Music(
    Note(..)
  , NoteMod(..)
  , NoteChunk(..)
  , Melody 
  , MelodyOptions(..)
  , generateMelody
  , mutateMelody
  , crossoverMelodies
  , printMelody
  ) where

import Data.Text (Text, unpack, pack)
import Data.List
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
  deriving Eq

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
, melodyMutateRatio :: Rational
, melodyBlockLength :: Int
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

  -- Как выяснилось, хаскоровский парсер хочет чтобы после паузы обязательно была продолжительность
  -- А так же в изначальном генераторе был косяк -- пауза-диез :D
  genNote = do 
    note <- uniform allNotes
    mode <- do 
      isgen <- fromList [(False, 1 - melodyNoteModRatio), (True, melodyNoteModRatio)]
      if (isgen && note /= NoteP) then Just <$> uniform [NoteBemol, NoteDiez]
        else return Nothing
    lngth <- do 
      isgen <- fromList [(False, 1 - melodyNoteLengthRatio), (True, melodyNoteLengthRatio)]
      if (isgen || note == NoteP) then Just <$> uniform [melodyNoteLengthMin .. melodyNoteLengthMax]
        else return Nothing
    return $ NoteChunk note mode lngth 

  genNoteLength = NoteLength <$> uniform [melodyLengthModMin .. melodyLengthModMax]

printMelody :: Melody -> String
printMelody = V.foldl' printChunk ""
  where 
  printChunk acc ch = acc ++ "" ++ case ch of 
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



mutateMelody :: MelodyOptions -> Melody -> IO Melody
mutateMelody MelodyOptions{..} melody = sliceNdice melody
  where 
    sliceNdice :: Melody -> IO Melody
    sliceNdice mel = diceBlock (changePoint (length (splitFold mel)) ) (splitFold mel) 
      where
        splitFold :: Melody -> [Melody]
        splitFold mel 
          | V.null mel = []
          | otherwise     = (V.take melodyBlockLength mel) : (splitFold (V.drop melodyBlockLength mel))

        changePoint :: Int -> IO Int
        changePoint length = do
          cP <- uniform [0 .. length]
          return cP

        diceBlock :: IO Int -> [Melody] -> IO Melody
        diceBlock changePoint melArr = do 
          cP <- changePoint
          let leftPart = take (cP - 1) melArr
          let rightPart = drop cP melArr
          newBlock <- generateMelody (makeOptsWithNeedLenght (V.length (melArr !! cP))) 2
          return $ V.concat $ leftPart ++ [newBlock] ++ rightPart

        makeOptsWithNeedLenght :: Int -> MelodyOptions
        makeOptsWithNeedLenght length = MelodyOptions {
            melodyLengthMin = length
          , melodyLengthMax = length
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
          , melodyBlockLength = 4
        }


crossoverMelodies :: MelodyOptions -> (Melody,Melody) -> IO (Melody, Melody)
crossoverMelodies MelodyOptions{..} (fstMel,sndMel) = changeThem (changePoints (fstMel,sndMel)) (fstMel,sndMel)
  where
    changePoints :: (Melody, Melody) -> IO (Int, Int) 
    changePoints (fstMel, sndMel) = do
      (fstCP :: Int) <- uniform [((V.length sndMel) - melodyLengthMin) .. (melodyLengthMax - (V.length sndMel))]
      (sndCP :: Int) <- uniform [(melodyLengthMin - fstCP) .. (melodyLengthMax - ((length fstMel) - fstCP))]
      return (fstCP, sndCP)

    changeThem :: IO (Int, Int) -> (Melody, Melody) -> IO (Melody, Melody)
    changeThem mPoints (fstMel, sndMel) = do
      points <- mPoints
      let fstCP = fst points
      let sndCP = snd points 
      let newFstMel = V.concat $ [(V.take fstCP fstMel)] ++ [(V.drop sndCP sndMel)]
      let newSndMel = V.concat $ [(V.take sndCP sndMel)] ++ [(V.drop fstCP fstMel)]
      return (newFstMel, newSndMel)
