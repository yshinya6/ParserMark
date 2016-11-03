module Main where

import CelloParser
import Control.Monad
import Text.Parsec
import Data.Char

main = do
  l <- getContents
  parseTest programParser l
