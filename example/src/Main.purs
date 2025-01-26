module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Audio.SoundFont (loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(..))
import RhythmGuitar.Network (loadDefaultChordShapes)
import RhythmGuitar.Audio (buildMidiChordMap)

import Editor.Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  instruments <- loadRemoteSoundFonts [AcousticGrandPiano, AcousticGuitarSteel]
  chordShapes <- loadDefaultChordShapes
  let
    initialAbc = Nothing
    chordMap = buildMidiChordMap chordShapes
  traverse_ (runUI Container.component { instruments, chordMap, initialAbc }) =<< HA.selectElement (QuerySelector "#embed-ps-div")
