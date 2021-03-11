let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210309/packages.dhall sha256:585332a8a11c6420d7287943f81bc2121746cdd352f2cf3f5ecf65053f2afcd3

let overrides = {=}

let additions =
      { event =
          { dependencies =
            [ "effect"
            , "filterable"
            , "nullable"
            , "unsafe-reference"
            , "js-timers"
            , "now"
            ]
          , repo = "https://github.com/thomashoneyman/purescript-event.git"
          , version = "a1b8c78f767b041841a88597affee42e2551d874"
          }
      }

in  upstream // overrides // additions
