module Halogen.Storybook
  ( Stories
  , StoryQuery
  , runStorybook
  , module Halogen.Storybook.Proxy
  ) where

import Prelude

import Effect.Aff (Aff, launchAff_)

import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Web.HTML.HTMLElement (HTMLElement)

import Global.Unsafe (unsafeDecodeURI, unsafeEncodeURI)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook.Proxy (ProxyS, proxy)
import Halogen.VDom.Driver (runUI)
import Foreign.Object as Object

import Routing.Hash (hashes)

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

type Slot = String

type HTML m = H.ParentHTML Query StoryQuery Slot m

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

renderSidebar :: forall m. Stories m -> State -> HTML m
renderSidebar stories { route } =
  HH.ul [ class_ "Storybook-nav" ] $
    mapFlipped (Object.keys stories) $ \name ->
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
    Just cmp -> HH.slot state.route cmp unit absurd
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

render :: forall m. Stories m -> State -> HTML m
render stories state = do
  HH.div [ class_ "Storybook" ]
    [ HH.a
        [ class_ "Storybook-logo"
        , HP.href ""
        ]
        [ HH.text "Halogen Storybook" ]
    , renderSidebar stories state
    , HH.div [ class_ "Storybook-main" ]
        [ renderMain stories state  ]
    ]

app :: forall m. Stories m -> H.Component HH.HTML Query Unit Void m
app stories =
  H.parentComponent
    { initialState: const initialState
    , render: render stories
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { route: "" }

  eval :: Query ~> H.ParentDSL State Query StoryQuery Slot Void m
  eval (RouteChange route next) = do
    void $ H.modify (\state -> state { route = route })
    pure next

-- | Takes stories config and mount element, and renders the storybook.
runStorybook
  :: Stories Aff
  -> HTMLElement
  -> Aff Unit
runStorybook stories body = do
  app' <- runUI (app stories) unit body
  void $ H.liftEffect $ hashes $ \_ next ->
    launchAff_ $ app'.query (H.action $ RouteChange $ unsafeDecodeURI next)
