{ name = "halogen-storybook"
, dependencies =
  [ "aff"
  , "arrays"
  , "const"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "halogen"
  , "js-uri"
  , "maybe"
  , "partial"
  , "prelude"
  , "routing"
  , "strings"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
