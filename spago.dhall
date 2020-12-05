{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-abc-editor"
, dependencies = [ "abc-melody"
                 , "abc-parser"
                 , "abc-scores"
                 , "console"
                 , "css"
                 , "effect"
                 , "halogen"
                 , "halogen-css"
                 , "halogen-components"
                 , "soundfonts"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
