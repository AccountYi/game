module Lib
  ( main
  ) where

import ClassyPrelude
import qualified Adapter.HTTP.Main as HTTP

main :: IO ()
main = 
    HTTP.main 3000
