{ name = "lights-out"
, dependencies =
  [ "arrays"
  , "effect"
  , "integers"
  , "linalg"
  , "maybe"
  , "pha"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
