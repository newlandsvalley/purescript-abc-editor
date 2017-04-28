module Main where

import App (foldp, initialState, view)
-- import Audio.SoundFont (AUDIO)
import FileIO.FileIO (FILEIO)
import VexTab.Score as VexScore
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

-- | Start and render the app
-- main :: ∀ fx. Eff (CoreEffects (fileio :: FILEIO, vt :: VexScore.VEXTAB| fx)) Unit
main :: Eff (CoreEffects (fileio :: FILEIO, vt :: VexScore.VEXTAB )) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
