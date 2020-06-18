{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-storybook"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-object"
  , "halogen"
  , "psci-support"
  , "routing"
  , "event"
  , "debug"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
