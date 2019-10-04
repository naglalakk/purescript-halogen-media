{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "const"
    , "dom-filereader"
    , "effect"
    , "foreign-generic"
    , "generics-rep"
    , "halogen"
    , "psci-support"
    , "uuid"
    , "web-events"
    , "web-file"
    ]
, packages =
    ./packages.dhall
}
