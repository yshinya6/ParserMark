module Main where

import Control.Monad
import Text.Parsec
import Data.Char
import Data.Time

main :: IO ()
main = do
  testInput <- getContents
  start <- getCurrentTime
  runParseTest testInput
  getCurrentTime >>= (\time -> return (diffUTCTime time start)) >>= (\ps -> print ps)

muToM :: String -> String
muToM s = insert $ splitAt 2 s
  where
    insert :: (String, String)-> String
    insert xs = fst xs ++ "." ++ snd xs ++ "[ms]"

runParseTest :: String -> IO()
runParseTest xs  = case (parse testParser "" xs) of
  Left err -> print err
  Right str -> putStr "success,"

{-Grammar Definition-}
testParser = many (string "aa" <|> string "bb")