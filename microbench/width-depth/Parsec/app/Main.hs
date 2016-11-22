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
runParseTest xs  = case (parse nonterminala "" xs) of
  Left err -> print err
  Right str -> putStr "success,"

{-Grammar Definition-}
nonterminala = try ((string "a") >> nonterminalb) <|> eof
nonterminalb = ((string "b") >> nonterminalc)
nonterminalc = ((string "c") >> nonterminald)
nonterminald = ((string "d") >> nonterminale)
nonterminale = ((string "e") >> nonterminalf)
nonterminalf = ((string "f") >> nonterminalg)
nonterminalg = ((string "g") >> nonterminalh)
nonterminalh = ((string "h") >> nonterminali)
nonterminali = ((string "i") >> nonterminalj)
nonterminalj = ((string "j") >> nonterminalk)
nonterminalk = ((string "k") >> nonterminall)
nonterminall = ((string "l") >> nonterminalm)
nonterminalm = ((string "m") >> nonterminaln)
nonterminaln = ((string "n") >> nonterminalo)
nonterminalo = ((string "o") >> nonterminalp)
nonterminalp = ((string "p") >> nonterminalq)
nonterminalq = ((string "q") >> nonterminalr)
nonterminalr = ((string "r") >> nonterminals)
nonterminals = ((string "s") >> nonterminalt)
nonterminalt = ((string "t") >> nonterminalu)
nonterminalu = ((string "u") >> nonterminalv)
nonterminalv = ((string "v") >> nonterminalw)
nonterminalw = ((string "w") >> nonterminalx)
nonterminalx = ((string "x") >> nonterminaly)
nonterminaly = ((string "y") >> nonterminalz)
nonterminalz = ((string "z") >> nonterminalA)
nonterminalA = ((string "A") >> nonterminalB)
nonterminalB = ((string "B") >> nonterminalC)
nonterminalC = ((string "C") >> nonterminalD)
nonterminalD = ((string "D") >> nonterminalE)
nonterminalE = ((string "E") >> nonterminalF)
nonterminalF = ((string "F") >> nonterminalG)
nonterminalG = ((string "G") >> nonterminalH)
nonterminalH = ((string "H") >> nonterminalI)
nonterminalI = ((string "I") >> nonterminalJ)
nonterminalJ = ((string "J") >> nonterminalK)
nonterminalK = ((string "K") >> nonterminalL)
nonterminalL = ((string "L") >> nonterminalM)
nonterminalM = ((string "M") >> nonterminalN)
nonterminalN = ((string "N") >> nonterminalO)
nonterminalO = ((string "O") >> nonterminalP)
nonterminalP = ((string "P") >> nonterminalQ)
nonterminalQ = ((string "Q") >> nonterminalR)
nonterminalR = ((string "R") >> nonterminalS)
nonterminalS = ((string "S") >> nonterminalT)
nonterminalT = ((string "T") >> nonterminalU)
nonterminalU = ((string "U") >> nonterminalV)
nonterminalV = ((string "V") >> nonterminalW)
nonterminalW = ((string "W") >> nonterminalX)
nonterminalX = ((string "X") >> nonterminalY)
nonterminalY = ((string "Y") >> nonterminalZ)
nonterminalZ = ((string "Z") >> nonterminal0)
nonterminal0 = ((string "0") >> nonterminal1)
nonterminal1 = ((string "1") >> nonterminal2)
nonterminal2 = ((string "2") >> nonterminal3)
nonterminal3 = ((string "3") >> nonterminal4)
nonterminal4 = ((string "4") >> nonterminal5)
nonterminal5 = ((string "5") >> nonterminal6)
nonterminal6 = ((string "6") >> nonterminal7)
nonterminal7 = ((string "7") >> nonterminal8)
nonterminal8 = ((string "8") >> nonterminal9)
nonterminal9 = ((string "9") >> nonterminala)
