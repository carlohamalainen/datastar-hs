module Main where

import Test.Hspec

import Hypermedia.Datastar.PatchElementsSpec qualified

main :: IO ()
main = hspec $ do
  Hypermedia.Datastar.PatchElementsSpec.spec
