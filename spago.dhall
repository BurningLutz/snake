{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "snake"
, dependencies =
    [ "aff"
    , "ansi"
    , "arrays"
    , "checked-exceptions"
    , "console"
    , "effect"
    , "foldable-traversable"
    , "integers"
    , "js-timers"
    , "node-buffer"
    , "node-process"
    , "node-streams"
    , "psci-support"
    , "random"
    , "refs"
    , "strings"
    , "transformers"
    , "tuples"
    , "typelevel-prelude"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
