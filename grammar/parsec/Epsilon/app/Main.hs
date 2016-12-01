module Main where

import EpsilonParser
import Control.Monad
import Text.Parsec
import Data.Char
import Data.Time

main = do
  l <- getContents
  start <- getCurrentTime
  runParseTest l
  getCurrentTime >>= (\time -> return (diffUTCTime time start)) >>= (\ps -> print ps) --(muToM (show (tdPicosec ps `div` 1000000))))

muToM :: String -> String
muToM s = insert $ splitAt 2 s
  where
    insert :: (String, String)-> String
    insert xs = fst xs ++ "." ++ snd xs ++ "[ms]"

runParseTest :: String -> IO()
runParseTest xs  = case (parse programParser "" xs) of
  Left err -> print err
  Right str -> print "success"
