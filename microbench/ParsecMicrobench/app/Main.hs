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
testParser = char 'a' <|> testParserb
testParserb = char 'b' <|> testParserc
testParserc = char 'c' <|> testParserd
testParserd = char 'd' <|> testParsere
testParsere = char 'e' <|> testParserf
testParserf = char 'f' <|> testParserg
testParserg = char 'g' <|> testParserh
testParserh = char 'h' <|> testParseri
testParseri = char 'i' <|> testParserj
testParserj = char 'j' <|> testParserk
testParserk = char 'k' <|> testParserl
testParserl = char 'l' <|> testParser
