module Main where

import Prelude

import Data.Number as Number
import Effect (Effect)
import Effect.Console (logShow)

circleArea :: Number -> Number
circleArea radius = Number.pi * radius * radius



main :: Effect Unit
main = do
  logShow (circleArea 10.0)
