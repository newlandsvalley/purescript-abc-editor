module View.CSS where

import Prelude (discard, ($), (#))
import Text.Smolder.Markup (Attribute)
import Pux.DOM.HTML.Attributes (style)
import CSS.Background (backgroundColor)
import CSS.Color (rgb, red, lightgrey, darkgrey)
import CSS.Display (display, block, float, floatLeft)
import CSS.Font (color, fontSize)
import CSS.Geometry (width, padding, margin)
import CSS.Size (px, em)
import CSS.TextAlign (textAlign, leftTextAlign, center)

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

leftPanelComponentStyle :: Attribute
leftPanelComponentStyle =
    style $ do
      margin (10.0 # px) (px 0.0) (px 20.0) (px 40.0)
      fontSize (em 1.2)

initialLabelStyle :: Attribute
initialLabelStyle =
    style $ do
      margin (0.0 # px) (px 0.0) (px 0.0) (px 40.0)
      fontSize (em 1.2)

inputStyle :: Attribute
inputStyle =
  style do
    padding (px 10.0) (px 0.0) (px 10.0) (px 0.0)
    -- margin (px 0.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.0)

errorHighlightStyle :: Attribute
errorHighlightStyle =
  style do
    color red

buttonStyle :: Boolean -> Attribute
buttonStyle enabled =
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

selectionStyle :: Attribute
selectionStyle =
  style do
    margin (px 0.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.0)
