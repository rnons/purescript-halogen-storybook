module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.StrMap as SM
import Data.Tuple (Tuple(Tuple))

import Halogen.Aff as HA

import Halogen.Storybook (Stories, runStorybook, proxy)

import Example.Input as ExpInput
import Example.Count as ExpCount

stories :: forall m. Stories m
stories = SM.fromFoldable
  [ Tuple "count" $ proxy $ ExpCount.component
  , Tuple "input" $ proxy $ ExpInput.component
  ]

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook stories body
