module App where

-- import CSS.Geometry (paddingTop, paddingBottom, marginLeft, marginRight, marginTop, width)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
-- import Control.Monad.Aff (Aff)
import Data.Abc (AbcTune)
import Data.Abc.Parser (PositionedParseError(..), parse)
import VexTab.Score as VexScore
import VexTab.Abc.Score (renderTune)
import Data.Array (length, slice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCharArray, toCharArray)
import FileIO.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import Prelude (bind, const, max, min, pure, ($), (#), (<>), (+), (-))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, canvas, div, h1, input, p, span, textarea)
import Text.Smolder.HTML.Attributes (type', id, accept, hidden, rows, cols, value)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import CSS.Size (px, em)
import CSS.Geometry (width, padding, margin)
import CSS.Font (color, fontFamily, fontSize)
-- import CSS.Common (auto)
import CSS.Background (backgroundColor)
import CSS.Color (rgb, red)
import CSS.Display (display, block, float, floatLeft)
import CSS.TextAlign (textAlign, leftTextAlign, center)


-- import Debug.Trace (trace, traceShow, traceShowM)

data Event
    = NoOp
    | Abc String
    | RequestFileUpload
    | RequestFileDownload
    | FileLoaded Filespec
    | VexInitialised Boolean -- is Vex initialised ?
    | VexRendered Boolean    -- is the abc rendered as a score ?
    | Reset

type State = {
    abc :: String
  , fileName :: Maybe String
  , tuneResult :: Either PositionedParseError AbcTune
  , vexInitialised :: Boolean
  , vexRendered :: Boolean
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
  }

foldp :: Event -> State -> EffModel State Event (fileio :: FILEIO, vt :: VexScore.VEXTAB)
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

-- | this is a horribly complicated type signature that I need to understand
-- | we cannot initialise Vex until we have rendered the Dom so initialise on first reference
-- ensureVexInitialised :: forall e. State -> Aff e (Maybe Event)
ensureVexInitialised state =
  if (state.vexInitialised) then
    do
      pure $ Just NoOp
  else
    do
      initialised <- liftEff initialiseVex
      pure $ Just (VexInitialised initialised)

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
      div do
        canvas ! id "vextab" $ text ""
    else
      div do
        canvas ! id "vextab" ! hidden "hidden" $ text ""


view :: State -> HTML Event
view state =
  div do
    h1 ! centreStyle $ text "ABC Editor"
    div ! leftPaneStyle $ do
      span ! leftPanelLabelStyle $ text "Load an ABC file:"
      input ! inputStyle ! type' "file" ! id "fileinput" ! accept ".abc, .txt"
           #! onChange (const RequestFileUpload)
      button #! onClick (const RequestFileDownload) $ text "save"
      button #! onClick (const Reset) $ text "reset"
    div do
      p $ text $ fromMaybe "no file chosen" state.fileName
      textarea ! taStyle ! cols "70" ! rows "15" ! value state.abc
        #! onInput (\e -> Abc (targetValue e) ) $ text ""
      viewParseError state
      viewCanvas state



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

{-
centreStyle : Attribute msg
centreStyle =
    style
        [ ( "text-align", "center" )
        , ( "margin", "auto" )
        ]
-}


leftPaneStyle :: Attribute
leftPaneStyle =
  style do
    width (px 350.0)
    float floatLeft

{-
leftPaneStyle :: forall a. Attribute a
leftPaneStyle =
    style
        [ Tuple "float" "left"
        , Tuple "width" "350px"(const Abc)
  style do
    margin (px 40.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.2)
-}



leftPanelLabelStyle :: Attribute
leftPanelLabelStyle =
    style $ do
      margin (40.0 # px) (px 0.0) (px 0.0) (px 40.0)
      fontSize (em 1.2)



{-
leftPanelLabelStyle : Attribute msg
leftPanelLabelStyle =
    style
        [ ( "margin-left", "40px" )
        , ( "margin-top", "40px" )
        , ( "font-size", "1.2em" )
        ]
-}

inputStyle :: Attribute
inputStyle =
  style do
    padding (px 10.0) (px 0.0) (px 10.0) (px 0.0)
    margin (px 0.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.0)

{-}
inputStyle :: forall a. Attribute a
inputStyle =
    style $ do
      paddingTop (10.0 # px)
      paddingBottom (10.0 # px)
      fontSize (1.0 # em)
      marginLeft (40.0 # px)
-}

{-
inputStyle : Attribute Msg
inputStyle =
    style
        [ ( "padding", "10px 0" )
        , ( "font-size", "1em" )
        , ( "margin-left", "40px" )
        ]
-}

errorHighlightStyle :: Attribute
errorHighlightStyle =
  style do
    color red
