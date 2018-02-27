module App where

-- import CSS.Geometry (paddingTop, paddingBottom, marginLeft, marginRight, marginTop, width)

import Network.HTTP.Affjax (AJAX)
import Audio.Midi.Player as MidiPlayer
import Audio.BasePlayer (PlaybackState(..)) as BasePlayer
import VexTab.Score as VexScore
import Audio.SoundFont (AUDIO, Instrument, loadPianoSoundFont)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Abc (AbcTune, ModifiedKeySignature, Mode(..), PitchClass(..), Accidental(..))
import Data.Abc.Canonical (fromTune)
import Data.Abc.Octave as Octave
import Data.Abc.Tempo (defaultTempo, getBpm, setBpm)
import Data.Abc.Transposition (transposeTo)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Metadata (getKeySig, getTitle)
import Data.Abc.Parser (PositionedParseError(..), parse, parseKeySignature)
import Data.Array (length, singleton, slice)
import Data.Either (Either(..), isLeft, isRight)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.String (fromCharArray, toCharArray)
import View.Transposition (keyMenuOptions)
import View.CSS
import JS.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import Prelude (bind, const, discard, id, max, min, not, pure, show, ($), (#), (==), (<>), (+), (-), (<<<))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (DOMEvent, onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (button, canvas, div, h1, input, label, p, span, select, textarea)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (text, (#!), (!), (!?))
import VexTab.Abc.Score (renderTune)


-- import Debug.Trace (trace, traceShow, traceShowM)

data Event
    = NoOp
    | Abc String
    | RequestLoadPianoFont String
    | FontLoaded Instrument
    | RequestFileUpload
    | RequestFileDownload
    | FileLoaded Filespec
    | VexInitialised Boolean -- is Vex initialised ?
    | VexRendered Boolean    -- is the abc rendered as a score ?
    | MoveOctave Boolean     -- true is Up one octave, false is Down
    | SetTempo Int           -- set the tempo to the required bpm
    | Transpose String       -- transpose to a new key
    | PlayerEvent MidiPlayer.Event
    | Clear

type State = {
    abc :: String
  , fileName :: Maybe String
  , tuneResult :: Either PositionedParseError AbcTune
  , vexInitialised :: Boolean
  , vexRendered :: Boolean
  , playerState :: MidiPlayer.State
}

-- | initialise VexTab
initialiseVex :: forall e. Eff (vt :: VexScore.VEXTAB | e) Boolean
initialiseVex =
  let
    config :: VexScore.Config
    config =
      { canvasDivId : "#vextab"
      , canvasX : 10
      , canvasY : 10
      , canvasWidth : 1300
      , scale : 0.8
      }
   in
     VexScore.initialise (config)

-- | there is no tune yet
nullTune :: Either PositionedParseError AbcTune
nullTune =
  Left (PositionedParseError { pos : 0, error : "" })

initialState :: State
initialState = {
    abc : ""
  , fileName : Nothing
  , tuneResult : nullTune
  , vexInitialised : false    -- we initialise on first reference
  , vexRendered : false
  , playerState : MidiPlayer.initialState []
  }


foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, fileio :: FILEIO, au :: AUDIO, vt :: VexScore.VEXTAB)
foldp NoOp state =  noEffects $ state
foldp (Abc s) state =  onChangedAbc s state
foldp (RequestLoadPianoFont fontDir) state =
  let
    effects =
      [
        do  -- request the fonts are loaded
          instrument <- loadPianoSoundFont fontDir
          pure $ Just (FontLoaded instrument)
      ]
  in
    {state: state, effects: effects}
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadTextFile "abcinput"
         pure $ Just (FileLoaded filespec)
     ]
  }
foldp (FileLoaded filespec) state =
  onChangedFile filespec state
foldp RequestFileDownload state =
   { state: state
     , effects:
       [ do
           let
             -- fileName = fromMaybe "unknown.abc" state.fileName
             fileName = getFileName state
             fsp = { name: fileName, contents : state.abc} :: Filespec
           res <- liftEff $ saveTextFile fsp
           pure $ (Just NoOp)
       ]
    }
foldp (FontLoaded instrument) state =
  let
    playerState = MidiPlayer.initialState (singleton instrument)
  in
    -- we need to react to a changed Euterpea after each instrument font loads.  This is because the user may edit
    -- the tne text to incorporate the new instrument names before loading their soundfonts
    noEffects $ state { playerState = playerState }
foldp Clear state =
  noEffects $ state { abc = ""
                    , fileName = Nothing
                    , tuneResult = nullTune
                    , vexRendered = false
                    }
foldp (VexInitialised initialised) state =
  noEffects $ state { vexInitialised = initialised }
foldp (VexRendered rendered) state =
  noEffects $ state { vexRendered = rendered }
foldp (MoveOctave isUp) state =
  let
    newState = changeTune (Octave.move isUp) state
  in
    onChangedAbc newState.abc newState
foldp (SetTempo bpm) state =
  let
    newState = changeTune (setBpm bpm) state
  in
    onChangedAbc newState.abc newState
foldp (Transpose newKey) state =
  let
    newState = transposeTune newKey state
  in
    onChangedAbc newState.abc newState
foldp (PlayerEvent e) state =
  MidiPlayer.foldp e state.playerState
    # mapEffects PlayerEvent
    # mapState \pst -> state { playerState = pst }

-- | make sure everything is notified if the ABC changes for any reason
-- | we'll eventually have to add effects
onChangedAbc  :: forall e. String -> State ->  EffModel State Event (vt :: VexScore.VEXTAB | e)
onChangedAbc abc state =
  let
    tuneResult =
      parse $ abc <> " \r\n"
    newState =
      state { tuneResult = tuneResult, abc = abc, vexRendered = false }
  in
    case tuneResult of
      Right tune ->
        {state: newState
          , effects:
            [
              ensureVexInitialised newState
            , do
                rendered <- liftEff $ renderTune tune
                pure $ Just (VexRendered rendered)
            , do
                pure $ Just (PlayerEvent (MidiPlayer.SetAbc tune))
            ]

        }
      Left err ->
        noEffects newState

-- | make sure everything is notified if a new file is loaded
onChangedFile :: forall e. Filespec -> State -> EffModel State Event (fileio :: FILEIO, vt :: VexScore.VEXTAB| e)
onChangedFile filespec state =
  let
    newState =
      state { fileName = Just filespec.name}
  in
    onChangedAbc filespec.contents newState

-- | get the file name from the previously loaded ABC or from the ABC itseelf
getFileName :: State -> String
getFileName state =
  case state.fileName of
    Just name ->
      name
    _ ->
      case state.tuneResult of
        Right tune ->
          (fromMaybe "untitled" $ getTitle tune) <> ".abc"
        _ ->
          "untitled.abc"

-- | we cannot initialise Vex until we have rendered the Dom so initialise on first reference
ensureVexInitialised :: forall e. State -> Aff (vt :: VexScore.VEXTAB | e) (Maybe Event)
ensureVexInitialised state =
  if (state.vexInitialised) then
    do
      pure $ Just NoOp
  else
    do
      initialised <- liftEff initialiseVex
      pure $ Just (VexInitialised initialised)

debugVex :: State -> HTML Event
debugVex state =
  do
    text ("vex rendered: " <> show state.vexRendered)
    text (" vex initialised: " <> show state.vexInitialised)


debugPlayer :: State -> HTML Event
debugPlayer state =
  do
    text ("player melody size: " <> (show $ length state.playerState.basePlayer.melody))

-- | transpose
transposeTune :: String -> State -> State
transposeTune s  =
  case parseKeySignature s of
    Right mks ->
      let
        ka = Accidentals.fromKeySig mks.keySignature
      in
        changeTune (transposeTo ka)
    Left _ ->
      id

-- | apply a function to change the ABC tune and save the state
changeTune :: (AbcTune -> AbcTune) -> State -> State
changeTune f state =
  case state.tuneResult of
    Right tune ->
      let
        newTune = f tune
        newAbc = fromTune newTune
      in
        state {  abc = newAbc, tuneResult = (Right newTune) }
    _ ->
      state

viewFileName :: State -> HTML Event
viewFileName state =
  case state.fileName of
    Just name ->
      text name
    _ ->
      text ""

-- | display a snippet of text with the error highlighted
viewParseError :: State -> HTML Event
viewParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = toCharArray state.abc
  in
    case state.tuneResult of
      Left (PositionedParseError pe) ->
        let
          -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
          startPhrase =
            max (pe.pos - textRange) 0
          errorPrefix =
            slice startPhrase pe.pos txt
          startSuffix =
            min (pe.pos + 1) (length txt)
          endSuffix =
            min (pe.pos + textRange + 1) (length txt)
          errorSuffix =
            slice startSuffix endSuffix txt
          errorChar =
            slice pe.pos (pe.pos + 1) txt
        in
          p do
              text $ pe.error <> " - "
              text $ fromCharArray errorPrefix
              span ! errorHighlightStyle $ text (fromCharArray errorChar)
              text $ fromCharArray errorSuffix
      _ ->
        text ""

viewCanvas :: State -> HTML Event
viewCanvas state =
  if (state.vexRendered) then
    div ! canvasStyle $ do
      canvas ! At.id "vextab" $ text ""
  else
    div ! canvasStyle $ do
      canvas ! At.id "vextab" ! At.hidden "hidden" $ text ""

-- | only display the player if we have a Melody
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.tuneResult of
    Right _ ->
      child PlayerEvent MidiPlayer.view $ state.playerState
    _ ->
      text ""

tempoSlider :: State -> HTML Event
tempoSlider state =
  (input !? isDisabled) (At.disabled "disabled") ! sliderStyle ! At.type' "range" ! At.min "10" ! At.max "300" ! At.value (show bpm)
        -- #! (\e -> SetTempo ((toIntTempo >>> targetValue) e) )
        #! onInput (\e -> SetTempo (targetTempo e) )
    where
      bpm =  case state.tuneResult of
        Right tune -> getBpm tune
        _ -> defaultTempo.bpm -- 120
      isDisabled = isLeft state.tuneResult

transpositionMenu :: State -> HTML Event
transpositionMenu state =
  let
    cMajor :: ModifiedKeySignature
    cMajor =
       { keySignature:  { pitchClass: C, accidental: Natural, mode: Major }, modifications: List.Nil }
  in
    case state.tuneResult of
      Right tune ->
        let
          mks = fromMaybe cMajor $ getKeySig tune
        in
          -- only offer transposition if we don't have strange Klezmer/Balkan type modes
          if (List.null mks.modifications) then
            do
              select ! selectionStyle #! onChange (\e -> Transpose (targetValue e) )
                $ (keyMenuOptions mks.keySignature)
            else
              select ! selectionStyle ! At.disabled "disabled" #! onChange (const NoOp )
                $ (keyMenuOptions cMajor.keySignature)
      _ ->
        do
          select ! selectionStyle ! At.disabled "disabled" #! onChange (const NoOp )
            $ (keyMenuOptions cMajor.keySignature)

-- | get the tempo from the DOM event as an integer defaukting to 120
targetTempo :: DOMEvent-> Int
targetTempo s =
  fromMaybe defaultTempo.bpm $ (fromString <<< targetValue) s

-- | is the player playing ?
isPlaying :: State -> Boolean
isPlaying state =
  let
    playbackState = state.playerState.basePlayer.playing
  in
    (playbackState == BasePlayer.PLAYING)

view :: State -> HTML Event
view state =
  let
    isEnabled = isRight state.tuneResult
  in
    div do
      h1 ! centreStyle $ text "ABC Editor"
      -- the options and buttons on the left
      div ! leftPaneStyle $ do
        div ! leftPanelComponentStyle $ do
          label ! labelAlignmentStyle $ do
            text "load an ABC file:"
          -- the label is a hack to allow styling of file input which is
          -- otherwise impossible - see https://stackoverflow.com/questions/572768/styling-an-input-type-file-button
          label ! inputLabelStyle ! At.className "hoverable" ! At.for "abcinput" $ text "choose"
          input ! inputStyle ! At.type' "file" ! At.id "abcinput" ! At.accept ".abc, .txt"
               #! onChange (const RequestFileUpload)
        div ! leftPanelComponentStyle $ do
          viewFileName state
        div ! leftPanelComponentStyle  $ do
          label ! labelAlignmentStyle $ do
            text  "save or clear ABC:"
          button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const RequestFileDownload) $ text "save"
          button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const Clear) $ text "clear"
        div ! leftPanelComponentStyle $ do
          label ! labelAlignmentStyle $ do
            text  "change octave:"
          (button !? (not isEnabled)) (At.disabled "disabled") ! (buttonStyle isEnabled) ! At.className "hoverable"
               #! onClick (const $ MoveOctave true) $ text "up"
          (button !? (not isEnabled)) (At.disabled "disabled") ! (buttonStyle isEnabled) ! At.className "hoverable"
               #! onClick (const $ MoveOctave false) $ text "down"
        div ! leftPanelComponentStyle $ do
          label ! labelAlignmentStyle $ do
            text "transpose to: "
          transpositionMenu state
        div ! leftPanelComponentStyle $ do
          label ! labelAlignmentStyle $ do
            text "change tempo:"
          tempoSlider state
        div ! leftPanelComponentStyle $ do
          viewPlayer state

      -- the editable text on the right
      div ! rightPaneStyle $ do
        -- p $ text $ fromMaybe "no file chosen" state.fileName
        textarea ! taStyle ! At.cols "70" ! At.rows "15" ! At.value state.abc
          ! At.spellcheck "false" ! At.autocomplete "false" ! At.autofocus "true"
          #! onInput (\e -> Abc (targetValue e) ) $ text ""
        viewParseError state
      -- the score
      viewCanvas state
