module Main where

import Prelude

import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Example.Count as ExpCount
import Example.Input as ExpInput
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: forall m. Stories m
stories = Object.fromFoldable
  [ Tuple "count" $ proxy $ ExpCount.component
  , Tuple "input" $ proxy $ ExpInput.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook stories body
