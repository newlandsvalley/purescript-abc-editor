module View.Transposition
  (keyMenuOptions) where

import Data.Abc (Accidental(..), KeySignature, Mode(..), Pitch(..), PitchClass(..))
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
basicKeys :: Array Pitch
basicKeys =
  [ pitch C Natural
  , pitch D Natural
  , pitch E Natural
  , pitch F Natural
  , pitch G Natural
  , pitch A Natural
  , pitch B Natural
  ]

-- | the predominant Flat keys used in major modes
flatKeys :: Array Pitch
flatKeys =
  [ pitch A Flat
  , pitch B Flat
  , pitch E Flat
  ]

-- | the predominant sharp keys used in minor modes
sharpKeys :: Array Pitch
sharpKeys =
  [ pitch C Sharp
  , pitch F Sharp
  , pitch G Sharp
  ]

-- | build a pitch
pitch :: PitchClass -> Accidental -> Pitch
pitch pc a =
  Pitch { pitchClass : pc, accidental : a}

-- | how a key is displayed in the menu
showKey :: Mode -> Pitch -> String
showKey mode (Pitch p) =
  let
    showAcc =
      case p.accidental of
        Sharp -> "#"
        Flat -> "b"
        _ -> ""
  in
    show p.pitchClass <> showAcc <> " " <> show mode
