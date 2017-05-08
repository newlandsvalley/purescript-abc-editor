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
import DOM.Node.NodeType (NodeType(..))
import Data.Abc (AbcTune)
import Data.Abc.Parser (PositionedParseError(..), parse)
import Data.Array (length, slice)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.String (fromCharArray, toCharArray)
import FileIO.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import Prelude (bind, const, discard, max, min, pure, show, ($), (#), (<>), (+), (-))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, canvas, div, h1, input, p, span, textarea)
import Text.Smolder.HTML.Attributes (type', id, accept, className, disabled, hidden, rows, cols, value)
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
      , canvasWidth : 1200
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
      canvas ! id "vextab" $ mempty
  else
    div do
      canvas ! id "vextab" ! hidden "hidden" $ text ""


-- | only display the player if we have a MIDI recording
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.playerState of
    Just pstate ->
      child PlayerEvent MidiPlayer.view $ pstate
    _ ->
      p $ text "player state is null"

{-}
view :: State -> HTML Event
view state =
  div do
    h1 ! centreStyle $ text "ABC Editor"
    div ! leftPaneStyle $ do
      span ! leftPanelLabelStyle $ do
        text "Load an ABC file:"
        input ! inputStyle ! type' "file" ! id "fileinput" ! accept ".abc, .txt"
             #! onChange (const RequestFileUpload)
      span ! leftPanelLabelStyle $ do
        text "Save or reset text:"
        button ! button1Style ! className "hoverable" #! onClick (const RequestFileDownload) $ text "save"
        button ! button1Style ! className "hoverable" #! onClick (const Reset) $ text "reset"
    div ! rightPaneStyle $ do
      p $ text $ fromMaybe "no file chosen" state.fileName
      textarea ! taStyle ! cols "70" ! rows "15" ! value state.abc
        #! onInput (\e -> Abc (targetValue e) ) $ text ""
      viewParseError state
      viewPlayer state
      viewCanvas state
      debugVex state
-}

view :: State -> HTML Event
view state =
  let
    isDisabled :: Boolean
    isDisabled = isLeft state.tuneResult
  in
    div do
      h1 ! centreStyle $ text "ABC Editor"
      div ! leftPaneStyle $ do
        span ! leftPanelLabelStyle $ do
          text "Load an ABC file:"
          input ! inputStyle ! type' "file" ! id "fileinput" ! accept ".abc, .txt"
               #! onChange (const RequestFileUpload)
        span ! leftPanelLabelStyle $ do
          text "save or reset text:"
          button ! button1Style ! className "hoverable" #! onClick (const RequestFileDownload) $ text "save"
          button ! button1Style ! className "hoverable" #! onClick (const Reset) $ text "reset"
        span ! leftPanelLabelStyle $ do
          text "change octave:"
          button ! button1Style ! className "hoverable" #! onClick (const NoOp) $ text "up"
          button ! button1Style ! className "hoverable" #! onClick (const NoOp) $ text "down"

          -- why can't I get !? to work???
          -- button !? isDisabled (disabled "disabled") ! button1Style ! className "hoverable" #! onClick (const NoOp) $ text "up"
          -- button !? isDisabled (disabled "disabled") ! button1Style ! className "hoverable" #! onClick (const NoOp) $ text "down"
          -- button ! className "hoverable" !? isDisabled (disabled "disabled") $ mempty

      div ! rightPaneStyle $ do
        p $ text $ fromMaybe "no file chosen" state.fileName
        textarea ! taStyle ! cols "70" ! rows "15" ! value state.abc
          #! onInput (\e -> Abc (targetValue e) ) $ mempty
        viewParseError state
        viewPlayer state
      viewCanvas state
        -- debugVex state

-- | experimental
optDisabled :: Boolean -> Attribute
optDisabled b =
  if b then
    disabled "disabled"
  else
    mempty


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
    width (px 350.0)
    float floatLeft

rightPaneStyle :: Attribute
rightPaneStyle =
  style do
    float floatLeft

leftPanelLabelStyle :: Attribute
leftPanelLabelStyle =
    style $ do
      margin (40.0 # px) (px 0.0) (px 0.0) (px 40.0)
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

button1Style :: Attribute
button1Style =
  style do
    margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
    fontSize (em 1.0)

{-
    [ class "hoverable"
    , buttonStyle isEnabled -- hasTopMargin
    -- , onClick msg
    , disabled (not isEnabled)
    ]
-}

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
