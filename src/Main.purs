module Main where

import App (Event(..), foldp, initialState, view)
import Network.HTTP.Affjax (AJAX)
import Audio.SoundFont (AUDIO)
import JS.FileIO (FILEIO)
import VexTab.Score as VexScore
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind, ($))
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Signal (Signal, constant)


initFont :: Signal Event
initFont = constant $ RequestLoadPianoFont "assets/soundfonts"

-- | Start and render the app
-- main :: ∀ fx. Eff (CoreEffects (fileio :: FILEIO, au :: AUDIO, vt :: VexScore.VEXTAB| fx)) Unit
main :: Eff (CoreEffects (ajax :: AJAX, fileio :: FILEIO, au:: AUDIO, vt :: VexScore.VEXTAB )) Unit
main = do

  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: [ initFont ]
    }

  renderToDOM "#app" app.markup app.input
