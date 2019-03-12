module Halogen.Storybook.Proxy
  ( proxy
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH

type Slot f o = H.Slot Query Message

type Query = Const Void

type Message = Void

data Action o = HandleChild o

type State i = i

type ChildSlots f o =
  ( child :: H.Slot f o Unit
  )

_child = SProxy :: SProxy "child"

proxy
  :: forall f i o m
   . H.Component HH.HTML f i o m
  -> H.Component HH.HTML Query i Message m
proxy innerComponent = H.mkComponent
  { initialState: identity
  , render: render innerComponent
  , eval: H.mkEval $ H.defaultEval
  }

render
  :: forall f i o m
   . H.Component HH.HTML f i o m
  -> State i
  -> H.ComponentHTML (Action o) (ChildSlots f o) m
render innerComponent state =
  HH.slot _child unit innerComponent state (Just <<< HandleChild)

handleAction
  :: forall f i o m
   . Action o
  -> H.HalogenM (State i) (Action o) (ChildSlots f o) Message m Unit
handleAction (HandleChild _)= pure unit
