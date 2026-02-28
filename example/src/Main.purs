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

import Editor.Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  instruments <- loadRemoteSoundFonts [AcousticGrandPiano, AcousticGuitarSteel]
  let
    initialAbc = Nothing
  traverse_ (runUI Container.component { instruments, initialAbc }) =<< HA.selectElement (QuerySelector "#embed-ps-div")
