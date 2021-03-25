{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "halogen-media-library"
, dependencies =
    [ "console"
    , "const"
    , "css"
    , "dom-filereader"
    , "effect"
    , "filesize"
    , "foreign-generic"
    , "halogen"
    , "halogen-css"
    , "generics-rep"
    , "psci-support"
    , "record"
    , "spec"
    , "uuid"
    , "web-events"
    , "web-file"
    , "web-xhr"
    ]
, packages =
    ./packages.dhall
}
