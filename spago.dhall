{ name = "profunctor-lenses"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "control"
  , "distributive"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
