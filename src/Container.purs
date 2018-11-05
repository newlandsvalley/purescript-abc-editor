module Container where

import Prelude

import Audio.SoundFont (Instrument, loadPianoSoundFont)
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import Effect.Aff (Aff)
import Data.Either (Either(..), either, isLeft)
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.List (List(..), null)
import Data.Array (cons, singleton) as A
import Data.Int (fromString)
import Data.MediaType (MediaType(..))
import Data.Abc.Parser (PositionedParseError, parseKeySignature)
import Data.Abc.Metadata (getKeySig, getTitle)
import Data.Abc (AbcTune)
import Data.Abc.Midi (toMidi)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Octave as Octave
import Data.Abc.Tempo (defaultTempo, getBpm, setBpm)
import Data.Abc.Accidentals (fromKeySig)
import Data.Abc.Transposition (transposeTo)
import VexFlow.Score (clearCanvas, renderTune, initialise) as Score
import VexFlow.Types (Config)
-- import VexFlow.Abc.Utils (canvasHeight)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..), HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.SimpleButtonComponent as Button
import Halogen.PlayerComponent as PC
import JS.FileIO (Filespec, saveTextFile)
import Partial.Unsafe (unsafePartial)

import Transposition (MenuOption(..), keyMenuOptions, cMajor, showKeySig)

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError AbcTune
  , fileName :: Maybe String
  , vexRendered :: Boolean
  }

data Query a =
    Init a
  | InitVex a
  | HandleABCFile FIC.Message a
  | HandleClearButton Button.Message a
  | HandleSaveButton Button.Message a
  | HandleNewTuneText ED.Message a
  | HandleMoveOctave Boolean a
  | HandleTempoInput Int a
  | HandleTranspositionKey String a
  | HandleTuneIsPlaying PC.Message a

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "choose file"
  , accept : MediaType ".abc"
  }

emptyTune :: AbcTune
emptyTune =
  { headers : Nil, body: Nil }

vexConfig :: Config
vexConfig =
  { canvasDivId : "vexflow"
  , canvasWidth : 1300
  , canvasHeight : 700
  , scale : 0.8
  }

-- the player is generic over a variety of playable sources of music
-- so we must specialize to MidiRecording
type PlayerQuery = PC.Query MidiRecording

type ChildQuery = Coproduct5 ED.Query FIC.Query Button.Query Button.Query PlayerQuery

-- slots and slot numbers
type FileInputSlot = Unit
type PlayerSlot = Unit
type ReplaceInstrumentsSlot = Unit
type ClearTextSlot = Unit
type SaveTextSlot = Unit
type EditorSlot = Unit

type ChildSlot = Either5 Unit Unit Unit Unit Unit

editorSlotNo :: CP.ChildPath ED.Query ChildQuery EditorSlot ChildSlot
editorSlotNo = CP.cp1

abcFileSlotNo :: CP.ChildPath FIC.Query ChildQuery FileInputSlot ChildSlot
abcFileSlotNo = CP.cp2

clearTextSlotNo :: CP.ChildPath Button.Query ChildQuery ClearTextSlot ChildSlot
clearTextSlotNo = CP.cp3

saveTextSlotNo :: CP.ChildPath Button.Query ChildQuery SaveTextSlot ChildSlot
saveTextSlotNo = CP.cp4

playerSlotNo :: CP.ChildPath PlayerQuery ChildQuery PlayerSlot ChildSlot
playerSlotNo = CP.cp5

component ::  H.Component HH.HTML Query Unit Void Aff
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { instruments: []
    , tuneResult: ED.nullTune
    , fileName: Nothing
    , vexRendered: false
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render state = HH.div_
    [ HH.h1
        [HP.class_ (H.ClassName "center") ]
        [HH.text "ABC Editor"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent")  ]
         [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "load ABC file:" ]
         , HH.slot' abcFileSlotNo unit (FIC.component abcFileInputCtx) unit (HE.input HandleABCFile)
         ]
      , HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          , HH.slot' saveTextSlotNo unit (Button.component "save") unit (HE.input HandleSaveButton)
          -- clear
          , HH.slot' clearTextSlotNo unit (Button.component "clear") unit (HE.input HandleClearButton)
          ]
      , HH.div
         -- shift octave
         [ HP.class_ (H.ClassName "leftPanelComponent")]
         [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "change octave:" ]
         -- up
         , renderOctaveButton true state
         -- down
         , renderOctaveButton false state
         ]
      , renderTempoSlider state
      , renderTranspositionMenu state
      , renderPlayer state
      ]
    -- right pane - editor
    , HH.div
        [ HP.class_ (H.ClassName "rightPane") ]
        [
          HH.slot' editorSlotNo unit ED.component unit (HE.input HandleNewTuneText)
        ]
    , renderCanvas state
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (Init next) = do
    -- instruments <- H.liftAff $  loadRemoteSoundFonts  [AcousticGrandPiano]
    instrument <- H.liftAff $ loadPianoSoundFont "assets/soundfonts"
    _ <- H.modify (\st -> st { instruments = A.singleton instrument } )
    eval (InitVex next)
  eval (InitVex next) = do
    -- we split initialisation into two because Vex requires a rendering step
    -- before it can be initialised
    _ <- H.liftEffect $ Score.initialise vexConfig
    pure next
  eval (HandleABCFile (FIC.FileLoaded filespec) next) = do
    _ <- H.modify (\st -> st { fileName = Just filespec.name } )
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent filespec.contents)
    _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
    pure next
  eval (HandleClearButton (Button.Toggled _) next) = do
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent "")
    pure next
  eval (HandleSaveButton (Button.Toggled _) next) = do
    maybeText <- H.query' editorSlotNo unit (H.request ED.GetText)
    state <- H.get
    let
      fileName = getFileName state
      text = fromMaybe "" maybeText
      fsp = { name: fileName, contents : text} :: Filespec
    _ <- H.liftEffect $ saveTextFile fsp
    pure next
  eval (HandleTempoInput bpm  next) = do
    state <- H.get
    let
      maybeText = changeTune (setBpm bpm) state
    _ <- onNewTuneText maybeText
    pure next
  -- eval (HandleMoveOctave isUp (Button.Toggled _) next) = do
  eval (HandleMoveOctave isUp next) = do
    state <- H.get
    let
      maybeText = changeTune (Octave.move isUp) state
    _ <- onNewTuneText maybeText
    pure next
  eval (HandleTranspositionKey keyString next) = do
    state <- H.get
    let
      maybeText = transposeTune keyString state
    _ <- onNewTuneText maybeText
    pure next
  eval (HandleNewTuneText (ED.TuneResult r) next) = do
    _ <- refreshPlayerState r
    let
      abcTune = either (\_ -> emptyTune) (identity) r
    _ <- H.liftEffect $ Score.clearCanvas
    rendered <- H.liftEffect $ Score.renderTune abcTune vexConfig
    _ <- H.modify (\st -> st { tuneResult = r, vexRendered = rendered } )
    pure next
  eval (HandleTuneIsPlaying (PC.IsPlaying p) next) = do
    -- we ignore this message, but if we wanted to we could
    -- disable any button that can alter the editor contents whilst the player
    -- is playing and re-enable when it stops playing
    {-
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateEnabled (not p))
    _ <- H.query' abcFileSlotNo unit $ H.action (FIC.UpdateEnabled (not p))
    _ <- H.query' clearTextSlotNo unit $ H.action (Button.UpdateEnabled (not p))
    -}
    pure next

-- | synchronize child components whenever we might have a change in tune text
-- | which emanates from OUTSIDE of the editor component
-- | (e.g. as the result of a tempo change, transposition or octave change)
onNewTuneText :: Maybe String -> H.ParentDSL State Query ChildQuery ChildSlot Void Aff Unit
onNewTuneText maybeText = do
  if (isJust maybeText)
   then do
     let
       tuneText = unsafePartial $ fromJust maybeText
     _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent tuneText)
     _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
     pure unit
   else
     pure unit

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState ::
       Either PositionedParseError AbcTune
    -> H.ParentDSL State Query ChildQuery ChildSlot Void Aff Unit
refreshPlayerState tuneResult = do
  _ <- either
    (\_ -> H.query' playerSlotNo unit $ H.action (PC.StopMelody))
    (\abcTune -> H.query' playerSlotNo unit $ H.action (PC.HandleNewPlayable (toPlayable abcTune)))
    tuneResult
  pure unit

-- helpers
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

-- | convert a tune to a format recognized by the player
toPlayable :: AbcTune -> MidiRecording
toPlayable abcTune =
   MidiRecording $ toMidi abcTune

-- rendering functions
renderOctaveButton :: Boolean -> State -> H.ParentHTML Query ChildQuery ChildSlot Aff
renderOctaveButton isUp state =
  let
    enabled =
      either (\_ -> false) (\_ -> true) state.tuneResult
    className =
          either (\_ -> "unhoverable") (\_ -> "hoverable") state.tuneResult
    label =
      if isUp
        then "up"
        else "down"
  in
    HH.button
      [ HE.onClick (HE.input_ (HandleMoveOctave isUp))
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]

renderPlayer ::  State -> H.ParentHTML Query ChildQuery ChildSlot Aff
renderPlayer state =
  case state.tuneResult of
    Right abcTune ->
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ HH.slot' playerSlotNo unit (PC.component (toPlayable abcTune) state.instruments) unit (HE.input HandleTuneIsPlaying)  ]
    Left err ->
      HH.div_
        [  ]

renderCanvas :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
renderCanvas state =
  HH.div
    [ HP.class_ (H.ClassName "canvasDiv")
    , HP.id_ "vexflow"
    ] []

renderTempoSlider :: State ->  H.ParentHTML Query ChildQuery ChildSlot Aff
renderTempoSlider state =
  let
    startBpm =
      case state.tuneResult of
        Right tune ->
          getBpm tune
        _ ->
          defaultTempo.bpm -- 120
    isDisabled = isLeft state.tuneResult
     -- | get the value from the slider result, defaulting to 120
    toTempo :: String -> Int
    toTempo s =
      fromMaybe defaultTempo.bpm $ fromString s
  in
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "change tempo:" ]

      , HH.input
          [ HE.onValueInput (HE.input HandleTempoInput <<< toTempo)
          , HP.type_ HP.InputRange
          , HP.id_ "tempo-slider"
          , HP.min 10.0
          , HP.max 300.0
          , HP.value (show startBpm)
          , HP.disabled isDisabled
          ]
      ]

renderTranspositionMenu :: State ->  H.ParentHTML Query ChildQuery ChildSlot Aff
renderTranspositionMenu state =
    let
      f :: ∀ p i. MenuOption -> HTML p i
      f mo =
        case mo of
          MenuOption text selected ->
            HH.option
              [ HP.disabled (selected) ]
              [ HH.text text]
      mks =
        case state.tuneResult of
          Right tune ->
            fromMaybe cMajor $ getKeySig tune
          _ ->
            cMajor
      enabled =
        case state.tuneResult of
          Right tune ->
            -- only offer transposition if we don't have strange Klezmer/Balkan type modes
            null mks.modifications
          _ ->
            false
    in
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ HH.label
           [ HP.class_ (H.ClassName "labelAlignment") ]
           [ HH.text "transpose to:" ]
        , HH.select
            [ HP.class_ $ ClassName "selection"
            , HP.id_  "transposition-menu"
            , HP.value (showKeySig mks.keySignature)
            , HP.enabled enabled
            , HE.onValueChange  (HE.input HandleTranspositionKey)
            ]
            (A.cons
              (HH.option [  ] [ HH.text (showKeySig mks.keySignature)])
              (map f $ keyMenuOptions mks.keySignature)
            )
        ]

-- Tune modication functions

-- | apply a function to change the ABC tune and return the new tune text
changeTune :: (AbcTune -> AbcTune) -> State -> Maybe String
changeTune f state =
  case state.tuneResult of
    Right tune ->
      Just (fromTune $ f tune)
    _ ->
      Nothing

-- | transpose
transposeTune :: String -> State -> Maybe String
transposeTune s state =
  case parseKeySignature s of
    Right mks ->
      changeTune (transposeTo $ fromKeySig mks.keySignature) state
    Left _ ->
      Nothing
