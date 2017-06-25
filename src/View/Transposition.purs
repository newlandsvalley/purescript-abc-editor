module View.Transposition
  (keyMenuOptions) where

import Data.Abc (Accidental(..), KeySignature, KeyAccidental(..), Mode(..), PitchClass(..))
import Data.Abc.Accidentals (fromKeySig)
import Prelude (show, (<>), (==), ($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.HTML.Attributes (selected)
import Text.Smolder.HTML (option)
import Data.Foldable (traverse_)

-- This module simply generates tre transposition menu options

-- | the menu options that are displayed for a given mode
-- | with pre-selection present if the target matches
-- | one of the options
keyMenuOptions :: ∀ a. KeySignature -> HTML a
keyMenuOptions targetKey =
  let
    kacc = fromKeySig targetKey
    target = showKey targetKey.mode kacc
    keys =
      case targetKey.mode of
        Major -> basicKeys <> flatKeys
        Minor -> basicKeys <> sharpKeys
        _ -> basicKeys
    f k =
      let
        next = showKey targetKey.mode k
      in
        if (next == target) then
          option ! selected "selected" $ text next
        else
          option $ text next
  in
    traverse_ f keys

-- | the set of basic keys
basicKeys :: Array KeyAccidental
basicKeys =
  [ ka C Natural
  , ka D Natural
  , ka E Natural
  , ka F Natural
  , ka G Natural
  , ka A Natural
  , ka B Natural
  ]

-- | the predominant Flat keys used in major modes
flatKeys :: Array KeyAccidental
flatKeys =
  [ ka A Flat
  , ka B Flat
  , ka E Flat
  ]

-- | the predominant sharp keys used in minor modes
sharpKeys :: Array KeyAccidental
sharpKeys =
  [ ka C Sharp
  , ka F Sharp
  , ka G Sharp
  ]

-- | build a key Accidental
ka :: PitchClass -> Accidental -> KeyAccidental
ka pc a =
  KeyAccidental { pitchClass : pc, accidental : a}

-- | how a key is displayed in the menu
showKey :: Mode -> KeyAccidental -> String
showKey mode (KeyAccidental kacc) =
  let
    showAcc =
      case kacc.accidental of
        Sharp -> "#"
        Flat -> "b"
        _ -> ""
  in
    show kacc.pitchClass <> showAcc <> " " <> show mode
