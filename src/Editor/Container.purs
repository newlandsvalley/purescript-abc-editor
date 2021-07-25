module Editor.Container where

import Prelude

import Audio.SoundFont (Instrument)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Abc (AbcTune)
import Data.Abc.Accidentals (fromKeySig)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Metadata (getKeySig, getTitle)
import Data.Abc.PlayableAbc (PlayableAbc(..))
import Data.Abc.Octave as Octave
import Text.Parsing.StringParser (ParseError)
import Data.Abc.Parser (parseKeySignature)
import Data.Abc.Tempo (defaultTempo, getBpm, setBpm)
import Data.Abc.Transposition (transposeTo)
import Data.Either (Either(..), either, hush, isLeft)
import Data.Int (fromString)
import Data.List (List(..), null)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.MediaType (MediaType(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Editor.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..), HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC
import JS.FileIO (Filespec, saveTextFile)
import Partial.Unsafe (unsafePartial)
import Editor.Transposition (MenuOption(..), keyMenuOptions, cMajor, showKeySig)
import VexFlow.Score (Renderer, clearCanvas, renderRightAlignedTune, renderTune, initialiseCanvas) as Score
import VexFlow.Types (Config)
import Editor.Window (print)
import Type.Proxy (Proxy(..))

type Slot = H.Slot Query Void

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either ParseError AbcTune
  , fileName :: Maybe String
  , vexRenderer :: Maybe Score.Renderer
  , vexRendered :: Boolean
  , vexAligned :: Boolean
  , initialAbc :: Maybe String
  }

type Input =
  { instruments :: Array Instrument
  , initialAbc :: Maybe String
  }


data Action =
    Init
  | HandleABCFile FIC.Message
  | HandleClear
  | HandleSave
  | HandleNewTuneText ED.Message
  | HandleMoveOctave Boolean
  | HandleTempoInput Int
  | HandleTranspositionKey String
  | HandleTuneIsPlaying PC.Message
  | HandleAlign
  | HandlePrint

-- | a simple button has no parameters and is greyed if there's no valid tune
data SimpleButtonType =
    Clear
  | Save
  | Align
  | Print

-- the only reason that we need Query at all is that we need to chain
-- InitDummy followed by InitVex and this is only possible with Queries.
-- Otherwise everything would be encoded as an Action.
-- And the reason for this is that Vex requires a Div element to me rendered
-- before it can be initialised.
--
-- Rendering takes place between the two initialisations.
data Query a =
    InitQuery a
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
  , titled : false
  }

type ChildSlots =
  ( editor :: ED.Slot Unit
  , abcfile :: FIC.Slot Unit
  , player :: (PC.Slot PlayableAbc) Unit
  )

_editor = Proxy :: Proxy "editor"
_abcfile = Proxy :: Proxy "abcfile"
_player = Proxy :: Proxy "player"

component :: ∀ o m. MonadAff m => H.Component Query Input o m
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
    , vexRendered: false
    , vexAligned: false
    , initialAbc: input.initialAbc
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div
    [ HP.id "abcEditor" ]
    [ HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent")  ]
         [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "load ABC file:" ]
         , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit HandleABCFile
         ]
      , HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          -- save
          , renderSimpleButton Save state
          -- clear
          , renderSimpleButton Clear state
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
        -- align
        , renderSimpleButton Align state
        -- print
        , renderSimpleButton Print state
        ]
      , renderTempoSlider state
      , renderTranspositionMenu state
      , renderPlayer state
      ]
    -- right pane - editor
    , HH.div
        [ HP.class_ (H.ClassName "rightPane") ]
        [
          HH.slot _editor unit ED.component unit HandleNewTuneText
        ]
    , renderScore state
    ]

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      -- state <- H.get
      -- defer to the query so we can chain them
      _ <- handleQuery (InitQuery unit)
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do
      _ <- H.modify (\st -> st { fileName = Just filespec.name } )
      _ <- H.tell _editor unit (ED.UpdateContent filespec.contents)
      _ <- H.tell _player unit PC.StopMelody
      pure unit
    HandleClear -> do
      _ <- H.modify (\st -> st { fileName = Nothing
                               , vexAligned = false
                               } )
      _ <- H.tell _editor unit (ED.UpdateContent "")
      pure unit
    HandleSave -> do
      maybeText <- H.request _editor unit ED.GetText
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
      case state.vexRenderer of
        Just renderer -> do
          _ <- H.liftEffect $ Score.clearCanvas $ renderer
          -- render the score with no RHS alignment
          rendered <- H.liftEffect $ Score.renderTune vexConfig renderer abcTune
          _ <- H.modify (\st -> st { tuneResult = r
                                   , vexRendered = rendered
                                   , vexAligned = false
                                   } )
          pure unit
        _ ->
          pure unit
    HandleTuneIsPlaying (PC.IsPlaying _) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      pure unit
    HandleAlign -> do
      state <- H.get
      case state.vexRenderer of
        Just renderer -> do
          let
            abcTune = either (\_ -> emptyTune) (identity) state.tuneResult
          _ <- H.liftEffect $ Score.clearCanvas renderer
          -- right align the score -- all the score right-hand sides align
          rendered <- H.liftEffect $ Score.renderRightAlignedTune vexConfig renderer abcTune
          _ <- H.modify (\st -> st { vexAligned = rendered } )
          pure unit
        _ ->
          pure unit
    HandlePrint -> do
      _ <-  H.liftEffect print
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    InitQuery next -> do
      -- a completely artificial state change, forcing our first render
      _ <- H.modify (\st -> st { vexRenderer = Nothing } )
      handleQuery (InitVex next)
    InitVex next -> do
      state <- H.get
      -- we split initialisation into two because Vex requires a rendering step
      -- before it can be initialised
      renderer <- H.liftEffect $ Score.initialiseCanvas vexConfig
      _ <- H.modify (\st -> st { vexRenderer = Just renderer } )
      -- now we can handle any input ABC text from the initialization
      _ <- onNewTuneText (state.initialAbc)
      pure (Just next)

-- | synchronize child components whenever we might have a change in tune text
-- | which emanates from OUTSIDE of the editor component
-- | (e.g. as the result of a tempo change, transposition or octave change)
onNewTuneText :: ∀ o m
  . MonadAff m
  => Maybe String
  -> H.HalogenM State Action ChildSlots o m Unit
onNewTuneText maybeText = do
  if (isJust maybeText)
   then do
     let
       tuneText = unsafePartial $ fromJust maybeText
     _ <- H.tell _editor unit (ED.UpdateContent tuneText)
     _ <- H.tell _player unit PC.StopMelody
     pure unit
   else
     pure unit

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o m
    . MonadAff m
    => Either ParseError AbcTune
    -> H.HalogenM State Action ChildSlots o m Unit
refreshPlayerState tuneResult = do
  _ <- either
     (\_ -> H.tell _player unit PC.StopMelody)
     (\abcTune -> H.tell _player unit (PC.HandleNewPlayable (toPlayable abcTune)))
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
toPlayable :: AbcTune -> PlayableAbc
toPlayable abcTune =
   PlayableAbc { abcTune: abcTune, bpm : 120, phraseSize : 0.7, generateIntro: false  }

-- rendering functions
renderSimpleButton :: ∀ m
  . MonadAff m
  => SimpleButtonType
  -> State
  -> H.ComponentHTML Action ChildSlots m
renderSimpleButton buttonType state =
  let
    label = case buttonType of
      Clear -> "clear"
      Save -> "save"
      Align -> "align"
      Print -> "print"
    action = case buttonType of
      Clear -> HandleClear
      Save ->  HandleSave
      Align -> HandleAlign
      Print -> HandlePrint
    enabled =
      either (\_ -> false) (\_ -> true) state.tuneResult
    className =
          either (\_ -> "unhoverable") (\_ -> "hoverable") state.tuneResult
  in
    HH.button
      [ HE.onClick \_ -> action
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]

renderOctaveButton :: ∀ m
  . MonadAff m
  => Boolean
  -> State
  -> H.ComponentHTML Action ChildSlots m
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
      [ HE.onClick \_ -> HandleMoveOctave isUp
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]

renderPlayer ::  ∀ m
  . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderPlayer state =
  case state.tuneResult of
    Right abcTune ->
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")
        , HP.id  "player-div"
        ]
        [ HH.slot _player unit (PC.component (toPlayable abcTune) state.instruments) unit HandleTuneIsPlaying ]
    Left _ ->
      HH.div_
        [  ]

renderScore :: ∀ m
  . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderScore state =
  HH.div
    [ HP.id "score"]
    [ renderTuneTitle state
      , HH.div
         [ HP.class_ (H.ClassName "canvasDiv")
         , HP.id "vexflow"
         ] []
    ]

renderTuneTitle :: ∀ m
  . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderTuneTitle state =
  case (hush state.tuneResult >>= getTitle) of
     Just title ->
         HH.h2
            [HP.id "tune-title" ]
            [HH.text title]
     _ ->
        HH.text ""

renderTempoSlider :: ∀ m
  . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
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
          [ HE.onValueInput  (HandleTempoInput <<< toTempo)
          , HP.type_ HP.InputRange
          , HP.id "tempo-slider"
          , HP.min 10.0
          , HP.max 300.0
          , HP.value (show startBpm)
          , HP.disabled isDisabled
          ]
      ]

renderTranspositionMenu :: ∀ m
  . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
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
          Right _ ->  -- tune
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
            , HP.id  "transposition-menu"
            , HP.value (showKeySig mks.keySignature)
            , HP.enabled enabled
            , HE.onValueChange HandleTranspositionKey
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
