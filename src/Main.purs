module Main where

import App (foldp, initialState, view)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (Canceler, launchAff)
import Audio.SoundFont (AUDIO, loadPianoSoundFont)
import FileIO.FileIO (FILEIO)
import VexTab.Score as VexScore
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

initialiseApp :: forall e. Eff (exception :: EXCEPTION | e) (Canceler e)
initialiseApp = do
  launchAff (loadPianoSoundFont "assets/soundfonts")

-- | Start and render the app
-- main :: ∀ fx. Eff (CoreEffects (fileio :: FILEIO, au :: AUDIO, vt :: VexScore.VEXTAB| fx)) Unit
main :: Eff (CoreEffects (fileio :: FILEIO, au:: AUDIO, vt :: VexScore.VEXTAB )) Unit
main = do

  _ <- initialiseApp

  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
