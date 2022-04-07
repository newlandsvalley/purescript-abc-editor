{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-abc-editor"
, dependencies =
  [ "abc-melody"
  , "abc-parser"
  , "abc-scores"
  , "aff"
  , "arrays"
  , "colors"
  , "console"
  , "css"
  , "dom-indexed"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-components"
  , "halogen-css"
  , "integers"
  , "js-fileio"
  , "lists"
  , "maybe"
  , "media-types"
  , "partial"
  , "prelude"
  , "soundfonts"
  , "string-parsers"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
