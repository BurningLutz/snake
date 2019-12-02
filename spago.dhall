{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff"
    , "ansi"
    , "arrays"
    , "checked-exceptions"
    , "console"
    , "effect"
    , "integers"
    , "js-timers"
    , "node-buffer"
    , "node-process"
    , "node-streams"
    , "psci-support"
    , "refs"
    , "transformers"
    , "typelevel-prelude"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
