module Example.Count where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = Increase a
  | Decrease a

type State = { value :: Int }

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
  initialState = { value: 0 }

  render :: State -> H.ComponentHTML Query () m
  render state =
    HH.div_
      [ HH.h3_
          [ HH.text "A counter" ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ HE.input_ Increase ]
              [ HH.text "+" ]
          ]
      , HH.div_
          [ HH.text $ show state.value ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ HE.input_ Decrease ]
              [ HH.text "-" ]
          ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void m
  eval (Increase next) =
    H.modify (\state -> state { value = state.value + 1 }) $> next
  eval (Decrease next) =
    H.modify (\state -> state { value = state.value - 1 }) $> next
