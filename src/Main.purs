module Main where


import CSS.Color (red)
import CSS.Font (color, fontSize)
import CSS.Geometry (paddingTop, paddingBottom, marginLeft, marginRight, marginTop, width)
import CSS.TextAlign (textAlign, leftTextAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Abc (AbcTune)
import Data.Abc.Parser (PositionedParseError(..), parse)
import VexTab.Score as VexScore
import VexTab.Abc.Canonical (toScoreText)
import VexTab.Abc.Translate (translate)
import Data.Array (length, slice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCharArray, toCharArray)
import FileIO.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import Prelude (Unit, bind, const, max, min, not, pure, ($), (#), (<>), (+), (-))
import Pux (EffModel, renderToDOM, start, noEffects)
import Pux.CSS (em, backgroundColor, px, rgb, style, center, display, block)
import Pux.Html (Html, Attribute, text, button, canvas, textarea, h1, span, div, input, p)
import Pux.Html.Attributes (rows, cols, hidden, placeholder, value, type_, id_, accept)
import Pux.Html.Events (onInput, onChange, onClick)
import Signal.Channel (CHANNEL)


-- import Debug.Trace (trace, traceShow, traceShowM)

data Action
    = NoOp
    | Abc String
    | RequestFileUpload
    | RequestFileDownload
    | FileLoaded Filespec
    | VexRendered Boolean    -- is the abc rendered as a score ?
    | Reset

type State = {
    abc :: String
  , fileName :: Maybe String
  , tuneResult :: Either PositionedParseError AbcTune
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

-- | run any initialisation code and then return the initial state
initialiseApp :: forall e. Eff (vt :: VexScore.VEXTAB | e) State
initialiseApp = do
  vexInitialised <- initialiseVex
  pure initialState


{- this is defined within Pux
type EffModel state action eff =
  { state :: state
  , effects :: Array (Aff (channel :: CHANNEL | eff) action)
  }
-}
initialState :: State
initialState = {
    abc : "initial value"
  , fileName : Nothing
  , tuneResult : Left (PositionedParseError { pos : 0, error : "not started" })
  , vexRendered : false
  }

update :: Action -> State -> EffModel State Action (fileio :: FILEIO, vt :: VexScore.VEXTAB)
update NoOp state =  noEffects $ state
update (Abc s) state =  onChangedAbc s state
update RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadTextFile
         pure $ FileLoaded filespec
     ]
  }
update RequestFileDownload state =
   { state: state
     , effects:
       [ do
           let
             fileName = fromMaybe "unknown.abc" state.fileName
             fsp = { name: fileName, contents : state.abc} :: Filespec
           res <- liftEff $ saveTextFile fsp
           pure $ NoOp
       ]
    }
update (FileLoaded filespec) state =
  onChangedFile filespec state
update Reset state =
  noEffects $ state { abc = "", fileName = Nothing }
update (VexRendered rendered) state =
  noEffects $ state { vexRendered = rendered }

-- | make sure everything is notified if the ABC changes for any reason
-- | we'll eventually have to add effects
onChangedAbc  :: forall e. String -> State ->  EffModel State Action (vt :: VexScore.VEXTAB | e)
onChangedAbc abc state =
  let
    tuneResult =
      parse $ abc <> " \r\n"
    newState =
      state { tuneResult = tuneResult, abc = abc }
  in
    case tuneResult of
      Right tune ->
        {state: newState
          , effects:
            [ do
                let
                  vexText = produceScore tune
                rendered <- liftEff $ VexScore.render vexText
                pure $ VexRendered rendered
            ]

        }
      Left err ->
        noEffects newState

-- | make sure everything is notified if a new file is loaded
onChangedFile :: forall e. Filespec -> State -> EffModel State Action (fileio :: FILEIO, vt :: VexScore.VEXTAB| e)
onChangedFile filespec state =
  let
    newState =
      state { fileName = Just filespec.name}
  in
    onChangedAbc filespec.contents newState

produceScore :: AbcTune -> String
produceScore tune =
  let
    eitherText =
      translate tune
  in
    case eitherText of
      Left err -> err
      Right score -> toScoreText score

-- | display a snippet of text with the error highlighted
viewParseError :: State -> Html Action
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
          p []
              [ text $ pe.error <> " - "
              , text $ fromCharArray errorPrefix
              , span [ errorHighlightStyle ] [ text $ fromCharArray errorChar ]
              , text $ fromCharArray errorSuffix
              ]
      _ ->
        text ""

view :: State -> Html Action
view state =
  div []
    [
      h1 [ centreStyle ] [ text "ABC Editor" ]
    , div
        [] [
             span [ leftPanelLabelStyle ] [ text "Load an ABC file:" ]
           , input
                [ type_ "file"
                , id_ "fileinput"
                -- FileIO port requires this exact id to be set
                , accept ".abc, .txt"
                --, onClick RequestFileUpload
                , onChange (const RequestFileUpload)
                , inputStyle
                ]
                  []
           , button [onClick (const RequestFileDownload)] [ text "save" ]
           , button [onClick (const Reset)] [ text "reset" ]
           ]
    , div
       []
         [
           p [] [ text $ fromMaybe "no file chosen" state.fileName]
         , textarea
            [ cols 70
            , rows 15
            , placeholder "abc"
            , id_ "abc"
            --, defaultValue state.abc
            , value state.abc
            , onInput (\f -> Abc f.target.value)
            , taStyle
            ]
              [ ]
         , viewParseError state,
         div [] []
           {-}
           [ canvas
              [ id_ "vextab"
              -- , hidden (isParseError model || isJust model.vextab.error),
              , hidden (not state.vexRendered)
              ]
                []
           ]
           -}
         ]
    ]


taStyle :: forall a. Attribute a
taStyle =
    style $ do
      paddingTop (10.0 # px)
      fontSize (1.5 # em)
      backgroundColor (rgb 243 246 198)
      textAlign leftTextAlign
      marginLeft (2.0 # px)
      marginRight (2.0 # px)
      display block
      -- fontFamily monospace
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

centreStyle :: forall a. Attribute a
centreStyle =
    style $ do
       textAlign center
       -- margin auto

{-
centreStyle : Attribute msg
centreStyle =
    style
        [ ( "text-align", "center" )
        , ( "margin", "auto" )
        ]
-}

leftPaneStyle :: forall a. Attribute a
leftPaneStyle =
    style $ do
      width (350.0 # px)
      -- float left

{-
leftPaneStyle :: forall a. Attribute a
leftPaneStyle =
    style
        [ Tuple "float" "left"
        , Tuple "width" "350px"
        ]
-}
leftPanelLabelStyle :: forall a. Attribute a
leftPanelLabelStyle =
    style $ do
      marginLeft (40.0 # px)
      marginTop (40.0 # px)
      fontSize (1.2 # em)

{-}
leftPanelLabelStyle : Attribute msg
leftPanelLabelStyle =
    style
        [ ( "margin-left", "40px" )
        , ( "margin-top", "40px" )
        , ( "font-size", "1.2em" )
        ]
-}

inputStyle :: forall a. Attribute a
inputStyle =
    style $ do
      paddingTop (10.0 # px)
      paddingBottom (10.0 # px)
      fontSize (1.0 # em)
      marginLeft (40.0 # px)

{-
inputStyle : Attribute Msg
inputStyle =
    style
        [ ( "padding", "10px 0" )
        , ( "font-size", "1em" )
        , ( "margin-left", "40px" )
        ]
-}

errorHighlightStyle :: forall a. Attribute a
errorHighlightStyle =
    style $ do
      color red

main :: Eff (channel :: CHANNEL, err :: EXCEPTION, fileio :: FILEIO, vt :: VexScore.VEXTAB ) Unit
main = do

  state <- initialiseApp

  app <- start
    { initialState: state
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
