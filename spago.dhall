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
    , "console"
    , "psci-support"
    , "halogen"
    , "generics-rep"
    , "argonaut"
    , "foreign-generic"
    , "const"
    , "web-file"
    , "web-events"
    ]
, packages =
    ./packages.dhall
}
