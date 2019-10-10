{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "effect"
    , "css"
    , "console"
    , "psci-support"
    , "halogen"
    , "halogen-css"
    , "generics-rep"
    , "argonaut"
    , "foreign-generic"
    , "const"
    , "web-file"
    , "web-events"
    , "dom-filereader"
    , "uuid"
    ]
, packages =
    ./packages.dhall
}
