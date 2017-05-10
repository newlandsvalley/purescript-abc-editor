module App where

-- import CSS.Geometry (paddingTop, paddingBottom, marginLeft, marginRight, marginTop, width)
import Data.Midi.Player as MidiPlayer
import VexTab.Score as VexScore
import Audio.SoundFont (AUDIO)
import CSS.Background (backgroundColor)
import CSS.Color (rgb, red, lightgrey, darkgrey)
import CSS.Display (display, block, float, floatLeft)
import CSS.Font (color, fontSize)
import CSS.Geometry (width, padding, margin)
import CSS.Size (px, em)
import CSS.TextAlign (textAlign, leftTextAlign, center)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Abc (AbcTune, ModifiedKeySignature, KeyAccidental)
import Data.Abc.Canonical (fromTune)
import Data.Abc.Octave as Octave
import Data.Abc.Tempo (defaultTempo, getBpm, setBpm)
import Data.Abc.Transposition (transposeTo)
import Data.Abc.Parser (PositionedParseError(..), parse)
import Data.Array (length, slice)
import Data.Either (Either(..), isLeft, isRight)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.Monoid (mempty)
import Data.String (fromCharArray, toCharArray)
import FileIO.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import Prelude (bind, const, discard, max, min, not, pure, show, ($), (#), (<>), (+), (-), (<<<))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (DOMEvent, onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, canvas, div, h1, input, p, span, textarea)
-- import Text.Smolder.HTML.Attributes (type', id, accept, className, disabled, hidden, rows, cols, value)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (Attribute, text, (#!), (!), (!?))
import VexTab.Abc.Score (renderTune)


-- import Debug.Trace (trace, traceShow, traceShowM)

data Event
    = NoOp
    | Abc String
    | RequestFileUpload
    | RequestFileDownload
    | FileLoaded Filespec
    | VexInitialised Boolean -- is Vex initialised ?
    | VexRendered Boolean    -- is the abc rendered as a score ?
    | MoveOctave Boolean     -- true is Up one octave, false is Down
    | SetTempo Int           -- set the tempo to the required bpm
    | PlayerEvent MidiPlayer.Event
    | Reset

type State = {
    abc :: String
  , fileName :: Maybe String
  , tuneResult :: Either PositionedParseError AbcTune
  , vexInitialised :: Boolean
  , vexRendered :: Boolean
  , playerState :: Maybe MidiPlayer.State
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


initialState :: State
initialState = {
    abc : ""
  , fileName : Nothing
  , tuneResult : Left (PositionedParseError { pos : 0, error : "not started" })
  , vexInitialised : false    -- we initialise on first reference
  , vexRendered : false
  , playerState : Nothing
  }


foldp :: Event -> State -> EffModel State Event (fileio :: FILEIO, au :: AUDIO, vt :: VexScore.VEXTAB)
foldp NoOp state =  noEffects $ state
foldp (Abc s) state =  onChangedAbc s state
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadTextFile
         pure $ Just (FileLoaded filespec)
     ]
  }
foldp RequestFileDownload state =
   { state: state
     , effects:
       [ do
           let
             fileName = fromMaybe "unknown.abc" state.fileName
             fsp = { name: fileName, contents : state.abc} :: Filespec
           res <- liftEff $ saveTextFile fsp
           pure $ (Just NoOp)
       ]
    }
foldp (FileLoaded filespec) state =
  onChangedFile filespec state
foldp Reset state =
  noEffects $ state { abc = "", fileName = Nothing, vexRendered = false }
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
foldp (PlayerEvent e) state =
  case state.playerState of
    Just pstate ->
      MidiPlayer.foldp e pstate
        # mapEffects PlayerEvent
        # mapState \pst -> state { playerState = Just pst }
    _ ->
      noEffects state

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
        {state: newState { playerState = Just MidiPlayer.initialState}
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
        noEffects newState { playerState = Nothing }

-- | make sure everything is notified if a new file is loaded
onChangedFile :: forall e. Filespec -> State -> EffModel State Event (fileio :: FILEIO, vt :: VexScore.VEXTAB| e)
onChangedFile filespec state =
  let
    newState =
      state { fileName = Just filespec.name}
  in
    onChangedAbc filespec.contents newState

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

-- | change the tempo
{-}
changeTempo :: Int -> State -> State
changeTempo bpm =
  changeTune (setBpm bpm)
  -}

-- | transpose
transposeTune :: KeyAccidental -> State -> State
transposeTune ka =
  changeTune (transposeTo ka)

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
        mempty

viewCanvas :: State -> HTML Event
viewCanvas state =
  if (state.vexRendered) then
    div ! centreStyle $ do
      canvas ! At.id "vextab" $ mempty
  else
    div do
      canvas ! At.id "vextab" ! At.hidden "hidden" $ text ""

-- | only display the player if we have a MIDI recording
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.playerState of
    Just pstate ->
      child PlayerEvent MidiPlayer.view $ pstate
    _ ->
      mempty


tempoSlider :: State -> HTML Event
tempoSlider state =
  div do
    (input !? isDisabled) (At.disabled "disabled") ! sliderStyle ! At.type' "range" ! At.min "10" ! At.max "300" ! At.value (show bpm)
       -- #! (\e -> SetTempo ((toIntTempo >>> targetValue) e) )
       #! onInput (\e -> SetTempo (targetTempo e) )
  where
    bpm =  case state.tuneResult of
      Right tune -> getBpm tune
      _ -> defaultTempo.bpm -- 120
    isDisabled = isLeft state.tuneResult

-- | get the tempo from the DOM event as an integer defaukting to 120
targetTempo :: DOMEvent-> Int
targetTempo s =
  fromMaybe defaultTempo.bpm $ (fromString <<< targetValue) s

-- | is the player playing ?
isPlaying :: State -> Boolean
isPlaying state =
  case state.playerState of
    Just ps -> ps.playing
    _ -> false

view :: State -> HTML Event
view state =
  let
    isEnabled = isRight state.tuneResult
  in
    div do
      h1 ! centreStyle $ text "ABC Editor"
      -- the options and buttons on the left
      div ! leftPaneStyle $ do
        span ! leftPanelLabelStyle $ do
          text "Load an ABC file:"
          input ! inputStyle ! At.type' "file" ! At.id "fileinput" ! At.accept ".abc, .txt"
               #! onChange (const RequestFileUpload)
        div ! leftPanelLabelStyle  $ do
          text  "save or reset ABC text:"
          button ! (button1Style true) ! At.className "hoverable" #! onClick (const RequestFileDownload) $ text "save"
          button ! (button1Style true) ! At.className "hoverable" #! onClick (const Reset) $ text "reset"
        div ! leftPanelLabelStyle $ do
          text  "change octave:"
          (button !? (not isEnabled)) (At.disabled "disabled") ! (button1Style isEnabled) ! At.className "hoverable"
               #! onClick (const $ MoveOctave true) $ text "up"
          (button !? (not isEnabled)) (At.disabled "disabled") ! (button1Style isEnabled) ! At.className "hoverable"
               #! onClick (const $ MoveOctave false) $ text "down"
        div ! leftPanelLabelStyle $ do
          text "change tempo:"
          tempoSlider state
        div ! leftPanelLabelStyle $ do
          viewPlayer state

      -- the editable text on the right
      div ! rightPaneStyle $ do
        p $ text $ fromMaybe "no file chosen" state.fileName
        textarea ! taStyle ! At.cols "70" ! At.rows "15" ! At.value state.abc
          #! onInput (\e -> Abc (targetValue e) ) $ mempty
        viewParseError state
      -- the score
      viewCanvas state
        -- debugVex state



taStyle :: Attribute
taStyle =
    style do
      padding (px 10.0) (px 0.0) (px 10.0) (px 0.0)
      fontSize (em 1.5)
      backgroundColor (rgb 243 246 198)
      textAlign leftTextAlign
      margin (px 0.0) (px 2.0) (px 0.0) (px 2.0)
      display block
      -- fontFamily [ "monospace" ]
      -- align center

{-}
        [ ( "padding", "10px 0" )
        , ( "font-size", "1.5em" )
        , ( "text-align", "left" )
        , ( "align", "center" )
        , ( "display", "block" )
        , ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        , ( "background-color", "#f3f6c6" )
        , ( "font-family", "monospace" )
        ]
    -}

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
    -- margin auto

leftPaneStyle :: Attribute
leftPaneStyle =
  style do
    width (px 420.0)
    float floatLeft

rightPaneStyle :: Attribute
rightPaneStyle =
  style do
    float floatLeft

leftPanelLabelStyle :: Attribute
leftPanelLabelStyle =
    style $ do
      margin (10.0 # px) (px 0.0) (px 10.0) (px 40.0)
      fontSize (em 1.2)

inputStyle :: Attribute
inputStyle =
  style do
    padding (px 10.0) (px 0.0) (px 10.0) (px 0.0)
    margin (px 0.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.0)

errorHighlightStyle :: Attribute
errorHighlightStyle =
  style do
    color red

button1Style :: Boolean -> Attribute
button1Style enabled =
  if enabled then
    style do
      margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
      fontSize (em 1.0)
  else
    style do
      margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
      fontSize (em 1.0)
      backgroundColor lightgrey
      color darkgrey

sliderStyle :: Attribute
sliderStyle =
  style do
    width (px 150.0)
    margin (px 0.0) (px 0.0) (px 0.0) (px 40.0)

{-
buttonStyle :: Boolean -> Attribute
buttonStyle enabled =
  style do
    backgroundColor lightgrey
    color darkgrey
    fontSize (em 1.0)
    margin (px 20.0) (px 0.0) (px 0.0) (px 0.0)
-}

{-
bStyle : Bool -> Bool -> Attribute msg
bStyle enabled hasTopMargin =
    let
        colour =
            if enabled then
                []
            else
                [ ( "background-color", "lightgray" )
                , ( "color", "darkgrey" )
                ]

        textSize =
            [ ( "font-size", "1em" ) ]

        marginTop =
            if hasTopMargin then
                [ ( "margin-top", "20px" ) ]
            else
                []
    in
        style (colour ++ textSize ++ marginTop)
-}
