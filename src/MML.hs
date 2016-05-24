module MML
(
  saveMelody
) where

import qualified Data.Music.Music as GM

import Haskore.Interface.MIDI.Render as Render
import Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Interface.MML

saveMelody :: GM.Melody   -- Мелодия в терминах нашей проги
  -> Int                  -- Начальная октава мелодии
  -> String               -- Название (путь к) файла. Формата "*.midi" 
  -> IO ()
saveMelody m o fp = Render.fileFromGeneralMIDIMusic fp mel 
  where
    mel = melodyToMidi m o 

melodyToMidi :: GM.Melody -> Int -> MidiMusic.T
melodyToMidi m oct = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano $ toMusic oct mml
  where
    mml = GM.printMelody m