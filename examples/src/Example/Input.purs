module Example.Input where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = InputName String a

type State = { name :: String }

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

  initialState :: State
  initialState = { name: "" }

  render :: State -> H.ComponentHTML Query () m
  render state =
    HH.div_
      [ HH.h3_
          [ HH.text "What's your name?" ]
      , HH.input
          [ HP.value state.name
          , HE.onValueInput (HE.input InputName)
          ]
      , HH.p_
          [ HH.text $ "Hello, " <> state.name ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void m
  eval (InputName name next) = next <$ do
    H.modify _{ name = name }
