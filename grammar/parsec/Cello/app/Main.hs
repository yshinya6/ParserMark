module Main where

import CelloParser
import Control.Monad
import Text.Parsec
import Data.Char
import System.Time

main = do
  l <- getContents
  start <- getClockTime
  parseTest programParser l
  getClockTime >>= (\time -> return (diffClockTimes time start)) >>= (\ps -> putStr (muToM (show (tdPicosec ps `div` 1000000))))

muToM :: String -> String
muToM s = insert $ splitAt 2 s
  where
    insert :: (String, String)-> String
    insert xs = fst xs ++ "." ++ snd xs ++ "[ms]"
