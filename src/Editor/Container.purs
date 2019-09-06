module Editor.Container where

import Prelude

import Audio.SoundFont (Instrument)
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Abc (AbcTune)
import Data.Abc.Accidentals (fromKeySig)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Metadata (getKeySig, getTitle)
import Data.Abc.Midi (toMidi)
import Data.Abc.Octave as Octave
import Data.Abc.Parser (PositionedParseError, parseKeySignature)
import Data.Abc.Tempo (defaultTempo, getBpm, setBpm)
import Data.Abc.Transposition (transposeTo)
import Data.Either (Either(..), either, hush, isLeft)
import Data.Int (fromString)
import Data.List (List(..), null)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Data.Const (Const)
import Halogen as H
import Editor.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..), HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC
import Halogen.SimpleButtonComponent as Button
import JS.FileIO (Filespec, saveTextFile)
import Partial.Unsafe (unsafePartial)
import Editor.Transposition (MenuOption(..), keyMenuOptions, cMajor, showKeySig)
import VexFlow.Abc.Alignment (rightJustify)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderScore, initialiseCanvas) as Score
import VexFlow.Types (Config, VexScore)
import Editor.Window (print)

type Slot = H.Slot (Const Void) Void

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError AbcTune
  , fileName :: Maybe String
  , vexRenderer :: Maybe Score.Renderer
  , vexScore :: VexScore
  , vexRendered :: Boolean
  , vexAligned :: Boolean
  }

type Input =
  { instruments :: Array Instrument }


data Action =
    Init
  | HandleABCFile FIC.Message
  | HandleClearButton Button.Message
  | HandleSaveButton Button.Message
  | HandleNewTuneText ED.Message
  | HandleMoveOctave Boolean
  | HandleTempoInput Int
  | HandleTranspositionKey String
  | HandleTuneIsPlaying PC.Message
  | HandleAlign
  | HandlePrint

-- the only reason that we need Query at all is that we need to chain
-- InitDummy followed by InitVex and this is only possible with Queries.
-- Otherwise everything would be encoded as an Action.
-- And the reason for this is that Vex requires a Div element to me rendered
-- before it can be initialised.
--
-- Rendering takes place between the two initialisations.
data Query a =
    InitDummy a
  | InitVex a

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "choose file"
  , accept :  mediaType (MediaType ".abc")
  }

emptyTune :: AbcTune
emptyTune =
  { headers : Nil, body: Nil }

vexConfig :: Config
vexConfig =
  { parentElementId : "vexflow"
  , width : 1300
  , height : 700
  , scale : 0.8
  , isSVG : true
  }

type ChildSlots =
  ( editor :: ED.Slot Unit
  , abcfile :: FIC.Slot Unit
  , clear :: Button.Slot Unit
  , savefile :: Button.Slot Unit
  , player :: (PC.Slot MidiRecording) Unit
  )

_editor = SProxy :: SProxy "editor"
_abcfile = SProxy :: SProxy "abcfile"
_clear = SProxy :: SProxy "clear"
_savefile = SProxy :: SProxy "savefile"
_player = SProxy :: SProxy "player"

component :: forall o. H.Component HH.HTML Query Input o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { instruments: input.instruments
    , tuneResult: ED.nullTune
    , fileName: Nothing
    , vexRenderer: Nothing
    , vexScore: Left ""
    , vexRendered: false
    , vexAligned: false
    }

  render :: State -> H.ComponentHTML Action ChildSlots Aff
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
         , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit (Just <<< HandleABCFile)
         ]
      , HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          , HH.slot _savefile unit (Button.component "save") unit (Just <<< HandleSaveButton)
          -- clear
          , HH.slot _clear unit (Button.component "clear") unit (Just <<< HandleClearButton)
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
      , HH.div
        -- print
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ HH.label
           [ HP.class_ (H.ClassName "labelAlignment") ]
           [ HH.text "score:" ]
        , renderAlignButton state
        , renderPrintButton state
        ]
      , renderTempoSlider state
      , renderTranspositionMenu state
      , renderPlayer state
      ]
    -- right pane - editor
    , HH.div
        [ HP.class_ (H.ClassName "rightPane") ]
        [
          HH.slot _editor unit ED.component unit (Just <<< HandleNewTuneText)
        ]
    , renderScore state
    ]

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o Aff Unit
  handleAction = case _ of
    Init -> do
      -- defer to the query so we can chain them
      _ <- handleQuery (InitDummy unit)
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do
      _ <- H.modify (\st -> st { fileName = Just filespec.name } )
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent filespec.contents)
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit
    HandleClearButton (Button.Toggled _) -> do
      _ <- H.modify (\st -> st { fileName = Nothing
                               , vexScore = Left ""
                               , vexAligned = false
                               } )
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent "")
      pure unit
    HandleSaveButton (Button.Toggled _) -> do
      maybeText <- H.query _editor unit $ H.request ED.GetText
      state <- H.get
      let
        fileName = getFileName state
        text = fromMaybe "" maybeText
        fsp = { name: fileName, contents : text} :: Filespec
      _ <- H.liftEffect $ saveTextFile fsp
      pure unit
    HandleTempoInput bpm -> do
      state <- H.get
      let
        maybeText = changeTune (setBpm bpm) state
      _ <- onNewTuneText maybeText
      pure unit
    HandleMoveOctave isUp -> do
      state <- H.get
      let
        maybeText = changeTune (Octave.move isUp) state
      _ <- onNewTuneText maybeText
      pure unit
    HandleTranspositionKey keyString -> do
      state <- H.get
      let
        maybeText = transposeTune keyString state
      _ <- onNewTuneText maybeText
      pure unit
    HandleNewTuneText (ED.TuneResult r) -> do
      _ <- refreshPlayerState r
      state <- H.get
      let
        abcTune = either (\_ -> emptyTune) (identity) r
        vexScore = Score.createScore vexConfig abcTune
      case state.vexRenderer of
        Just renderer -> do
          _ <- H.liftEffect $ Score.clearCanvas $ renderer
          -- render the score with no RHS alignment
          rendered <- H.liftEffect $ Score.renderScore vexConfig renderer vexScore
          _ <- H.modify (\st -> st { tuneResult = r
                                   , vexRendered = rendered
                                   , vexScore = vexScore
                                   , vexAligned = false
                                   } )
          pure unit
        _ ->
          pure unit
    HandleTuneIsPlaying (PC.IsPlaying p) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      pure unit
    HandleAlign -> do
      state <- H.get
      case state.vexRenderer of
        Just renderer -> do
          _ <- H.liftEffect $ Score.clearCanvas renderer
          -- right justify the score
          let
            justifiedScore = rightJustify vexConfig.width vexConfig.scale state.vexScore
          rendered <- H.liftEffect $ Score.renderScore vexConfig renderer justifiedScore
          _ <- H.modify (\st -> st { vexAligned = true } )
          pure unit
        _ ->
          pure unit
    HandlePrint -> do
      _ <-  H.liftEffect print
      pure unit

handleQuery :: ∀ o a . Query a -> H.HalogenM State Action ChildSlots o Aff (Maybe a)
handleQuery = case _ of
  InitDummy next -> do
    -- a completely artificial state change, forcing our first render
    _ <- H.modify (\st -> st { vexRenderer = Nothing } )
    handleQuery (InitVex next)
  InitVex next -> do
    -- we split initialisation into two because Vex requires a rendering step
    -- before it can be initialised
    renderer <- H.liftEffect $ Score.initialiseCanvas vexConfig
    _ <- H.modify (\st -> st { vexRenderer = Just renderer } )
    pure (Just next)

-- | synchronize child components whenever we might have a change in tune text
-- | which emanates from OUTSIDE of the editor component
-- | (e.g. as the result of a tempo change, transposition or octave change)
onNewTuneText :: ∀ o. Maybe String -> H.HalogenM State Action ChildSlots o Aff Unit
onNewTuneText maybeText = do
  if (isJust maybeText)
   then do
     let
       tuneText = unsafePartial $ fromJust maybeText
     -- _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent tuneText)
     -- _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
     _ <- H.query _editor unit $ H.tell (ED.UpdateContent tuneText)
     _ <- H.query _player unit $ H.tell PC.StopMelody
     pure unit
   else
     pure unit

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o.
       Either PositionedParseError AbcTune
    -> H.HalogenM State Action ChildSlots o Aff Unit
refreshPlayerState tuneResult = do
  _ <- either
     (\_ -> H.query _player unit $ H.tell PC.StopMelody)
     (\abcTune -> H.query _player unit $ H.tell (PC.HandleNewPlayable (toPlayable abcTune)))
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
renderOctaveButton :: Boolean -> State -> H.ComponentHTML Action ChildSlots Aff
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
      [ HE.onClick \_ -> Just (HandleMoveOctave isUp)
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]

renderPlayer ::  State -> H.ComponentHTML Action ChildSlots Aff
renderPlayer state =
  case state.tuneResult of
    Right abcTune ->
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")
        , HP.id_  "player-div"
        ]
        [ HH.slot _player unit (PC.component (toPlayable abcTune) state.instruments) unit (Just <<< HandleTuneIsPlaying) ]
    Left err ->
      HH.div_
        [  ]

renderAlignButton :: State -> H.ComponentHTML Action ChildSlots Aff
renderAlignButton state =
  let
    enabled =
      either (\_ -> false) (\_ -> not state.vexAligned) state.tuneResult
    className =
      if enabled then "hoverable" else "unhoverable"
  in
    HH.button
      [ HE.onClick \_ -> Just HandleAlign
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text "align" ]

renderPrintButton :: State -> H.ComponentHTML Action ChildSlots Aff
renderPrintButton state =
  let
    enabled =
      either (\_ -> false) (\_ -> true) state.tuneResult
    className =
      if enabled then "hoverable" else "unhoverable"
  in
    HH.button
      [ HE.onClick \_ -> Just HandlePrint
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text "print" ]

renderScore :: State -> H.ComponentHTML Action ChildSlots Aff
renderScore state =
  HH.div
    [ HP.id_ "score"]
    [ renderTuneTitle state
      , HH.div
         [ HP.class_ (H.ClassName "canvasDiv")
         , HP.id_ "vexflow"
         ] []
    ]

renderTuneTitle :: State -> H.ComponentHTML Action ChildSlots Aff
renderTuneTitle state =
  case (hush state.tuneResult >>= getTitle) of
     Just title ->
         HH.h2
            [HP.id_ "tune-title" ]
            [HH.text title]
     _ ->
        HH.text ""

renderTempoSlider :: State -> H.ComponentHTML Action ChildSlots Aff
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
          [ HE.onValueInput  (Just <<< HandleTempoInput <<< toTempo)
          , HP.type_ HP.InputRange
          , HP.id_ "tempo-slider"
          , HP.min 10.0
          , HP.max 300.0
          , HP.value (show startBpm)
          , HP.disabled isDisabled
          ]
      ]

renderTranspositionMenu :: State -> H.ComponentHTML Action ChildSlots Aff
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
            , HE.onValueChange (Just <<< HandleTranspositionKey)
            ]
            (map f $ keyMenuOptions mks.keySignature)
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
