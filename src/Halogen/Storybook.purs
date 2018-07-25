module Halogen.Storybook
  ( Stories
  , StoryQuery
  , runStorybook
  , module Halogen.Storybook.Proxy
  ) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, launchAff_)
import Foreign.Object as Object
import Global.Unsafe (unsafeDecodeURI, unsafeEncodeURI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook.Proxy (ProxyS, proxy)
import Halogen.VDom.Driver (runUI)
import Routing.Hash (hashes)
import Web.HTML.HTMLElement (HTMLElement)

data Query a
  = RouteChange String a

type State =
  { route :: String
  }

type StoryQuery = ProxyS (Const Void) Unit

-- | Stories config, each story consists of a story name and a component.
-- | Note the component needs to be proxied explicitly.
-- |
-- | ```
-- | stories :: forall m. Stories m
-- | stories = Object.fromFoldable
-- |   [ Tuple "count" $ proxy $ ExpCount.component
-- | ```
type Stories m = Object.Object (H.Component HH.HTML StoryQuery Unit Void m)

type Slot = ( story :: H.Slot StoryQuery Void String )

type HTML m = H.ComponentHTML Query Slot m

type Config m =
  { stories :: Stories m
  , logo :: Maybe HH.PlainHTML
  }

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

renderSidebar :: forall m. Stories m -> State -> HTML m
renderSidebar stories { route } =
  HH.ul [ class_ "Storybook-nav" ] $
    mapFlipped (Array.filter (not <<< String.null) $ Object.keys stories) $ \name ->
      HH.li_
      [ HH.a
        [ class_ if route == name then linkActiveClass else linkClass
        , HP.href $ "#" <> unsafeEncodeURI name
        ]
        [ HH.text name ]
      ]
  where
    linkClass = "Storybook-link"
    linkActiveClass = linkClass <> " is-active"

renderMain :: forall m. Stories m -> State -> HTML m
renderMain stories state =
  case Object.lookup state.route stories of
    Just cmp -> HH.slot (SProxy :: SProxy "story") state.route cmp unit absurd
    _ ->
      HH.div_
      [ HH.h2_ [ HH.text "Hello world" ]
      , HH.p_
        [ HH.text "This site is powered by "
        , HH.a
          [ HP.href "https://github.com/rnons/purescript-halogen-storybook" ]
          [ HH.text "halogen-storybook" ]
        , HH.text "."
        ]
      ]

render :: forall m. Config m -> State -> HTML m
render { stories, logo } state =
  HH.div [ class_ "Storybook" ]
  [ HH.a
    [ class_ "Storybook-logo"
    , HP.href "#"
    ]
    [ case logo of
        Nothing -> HH.text "Halogen Storybook"
        Just logo' -> HH.fromPlainHTML logo'
    ]
  , renderSidebar stories state
  , HH.div [ class_ "Storybook-main" ]
    [ renderMain stories state  ]
  ]

app :: forall m. Config m -> H.Component HH.HTML Query Unit Void m
app config =
  H.component
    { initialState: const initialState
    , render: render config
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = { route: "" }

  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval (RouteChange route next) = do
    void $ H.modify (\state -> state { route = route })
    pure next

-- | Takes stories config and mount element, and renders the storybook.
runStorybook
  :: Config Aff
  -> HTMLElement
  -> Aff Unit
runStorybook config body = do
  app' <- runUI (app config) unit body
  void $ H.liftEffect $ hashes $ \_ next ->
    launchAff_ $ app'.query (H.action $ RouteChange $ unsafeDecodeURI next)
