{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "bard"
, dependencies =
    [ "console"
    , "css"
    , "effect"
    , "foreign"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    , "record"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
