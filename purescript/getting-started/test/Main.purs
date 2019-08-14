module Test.Main where

import Prelude
import Euler (answer)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert (answer == 233168)
