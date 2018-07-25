module Example.Index where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Query = Const Void

type State = Unit

initialState :: State
initialState = unit

render :: forall m. State -> H.ComponentHTML Query () m
render state =
  HH.div_
  [ HH.h3_
    [ HH.text "This is a customized index page" ]
  , HH.p_
    [ HH.text "See "
    , HH.a
      [ HP.href "https://github.com/rnons/purescript-halogen-storybook" ]
      [ HH.text "README" ]
    , HH.text " for details."
    ]
  ]

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> H.HalogenM State Query () Void m
  eval = absurd <<< unwrap
