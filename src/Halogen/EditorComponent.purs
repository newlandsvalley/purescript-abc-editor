module Halogen.EditorComponent where

import Prelude

import Data.Either (Either(..), either)
import Data.String (null) as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Array (length, slice) as A
import Data.Abc.Parser (PositionedParseError(..), parse)
import Data.Abc (AbcTune)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen (IProp)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.CSS (style)
import CSS (color)
import Color (rgb)


type State =
  { text :: String
  , parseError :: Maybe PositionedParseError
  , isEnabled :: Boolean
  }

data Query a =
    UpdateContent String a
  | UpdateEnabled Boolean a
  | GetText (String -> a)

data Message = TuneResult (Either PositionedParseError AbcTune)

-- | there is no tune yet
nullTune :: Either PositionedParseError AbcTune
nullTune =
  Left (PositionedParseError { pos : 0, error : "" })

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { text : ""
    , parseError : Nothing
    , isEnabled : true
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.textarea
         [ HP.rows 12
         , HP.cols 60
         , HP.autofocus true
         , HP.value state.text
         , HP.class_ $ ClassName "abcEdit"
         , HP.enabled state.isEnabled
         -- , HP.wrap false
         , HE.onValueInput (HE.input UpdateContent)
         ]
      , renderParseError state
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    UpdateContent s next -> do
      let
        tuneResult =
          if S.null s then
            nullTune
          else
            parse (s <> " \r\n")
        parseError = either Just (\success -> Nothing) tuneResult
      _ <- H.modify (\state -> state {text = s, parseError = parseError})
      H.raise $ TuneResult tuneResult
      pure next
    UpdateEnabled isEnabled next -> do
      _ <- H.modify (\state -> state {isEnabled = isEnabled})
      pure next
    GetText reply -> do
      state <- H.get
      pure (reply state.text)

renderParseError :: State -> H.ComponentHTML Query
renderParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = toCharArray state.text
  in
    case state.parseError of
      Just (PositionedParseError pe) ->
        if (S.null state.text) then
          HH.div_ []
        else

          let
            -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
            startPhrase =
              max (pe.pos - textRange) 0
            errorPrefix =
              A.slice startPhrase pe.pos txt
            startSuffix =
              min (pe.pos + 1) (A.length txt)
            endSuffix =
              min (pe.pos + textRange + 1) (A.length txt)

            errorSuffix =
              A.slice startSuffix endSuffix txt
            errorChar =
              A.slice pe.pos (pe.pos + 1) txt
          in
            HH.p_
              [ HH.text $ pe.error <> " - "
              , HH.text $ fromCharArray errorPrefix
              , HH.span
                 [ errorHighlightStyle ]
                 [ HH.text (fromCharArray errorChar) ]
              , HH.text $ fromCharArray errorSuffix
              ]
      _ ->
        HH.div_ []

errorHighlightStyle :: ∀ i r. IProp (style :: String | r) i
errorHighlightStyle =
  style do
    color $ (rgb 255 0 0)
