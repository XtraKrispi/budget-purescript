{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "budget-ui"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "formatters"
    , "generics-rep"
    , "halogen"
    , "halogen-bootstrap4"
    , "halogen-css"
    , "js-timers"
    , "numbers"
    , "prelude"
    , "psci-support"
    , "read"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "transformers"
    , "web-html"
    , "web-storage"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
