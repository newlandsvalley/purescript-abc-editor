module Editor.Transposition
  (MenuOption(..), keyMenuOptions, cMajor, showKeySig) where

import Data.Abc (Accidental(..), ModifiedKeySignature, KeySignature, Mode(..), Pitch(..), PitchClass(..))
import Data.Abc.Accidentals (fromKeySig)
import Prelude (map, show, (<>), (==))
import Data.List (List(..))

-- | a menu option is a string representing the option and a boolean indicating
-- | whether it is selected
data MenuOption =
  MenuOption String Boolean

-- | The C major key signature
cMajor :: ModifiedKeySignature
cMajor =
     { keySignature:  { pitchClass: C, accidental: Natural, mode: Major }, modifications: Nil }

-- | how a key signature is displayed in the menu
showKeySig :: KeySignature -> String
showKeySig ks =
  showKey ks.mode (pitch ks.pitchClass ks.accidental)

-- This module simply generates the transposition menu options

-- | the menu options that are displayed for a given mode
-- | with pre-selection present if the target matches
-- | one of the options
keyMenuOptions :: KeySignature -> Array MenuOption
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
          MenuOption next true
        else
          MenuOption next false
  in
    map f keys

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
