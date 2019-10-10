{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "halogen-media-library"
, dependencies =
    [ "argonaut"
    , "console"
    , "const"
    , "css"
    , "dom-filereader"
    , "effect"
    , "foreign-generic"
    , "generics-rep"
    , "halogen"
    , "halogen-css"
    , "generics-rep"
    , "psci-support"
    , "uuid"
    , "web-events"
    , "web-file"
    , "web-xhr"
    ]
, packages =
    ./packages.dhall
}
