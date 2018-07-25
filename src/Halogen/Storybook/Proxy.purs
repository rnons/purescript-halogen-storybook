-- | A proxy that hides both the Query and Message of wrapped component.
-- | Adapted from `Halogen.Component.Proxy`.
module Halogen.Storybook.Proxy
  ( ProxyS
  , proxy
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH

data ProxyS f i a
  = Query (Coyoneda f a)

-- | A proxy that hides both the Query and Message of wrapped component.
proxy
  :: forall f i o m
  . H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS (Const Void) i) i Void m
proxy = proxyEval (const (absurd <<< un Const))

proxyEval
  :: forall f g i o m
   . (forall a b. (b -> a) -> g b -> H.HalogenM i (ProxyS g i) (child :: H.Slot f o Unit) Void m a)
  -> H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS g i) i Void m
proxyEval evalQuery component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    render :: i -> H.ComponentHTML (ProxyS g i) (child :: H.Slot f o Unit) m
    render i = HH.slot (SProxy :: SProxy "child") unit component i (const Nothing)

    eval :: ProxyS g i ~> H.HalogenM i (ProxyS g i) (child :: H.Slot f o Unit) Void m
    eval (Query iq) = unCoyoneda evalQuery iq
