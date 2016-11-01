module CelloParser where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "=.:/<>=!;|&+-*"
              , opLetter = oneOf "=.:/<>=&|;+-*!"
              , reservedOpNames = ["=", "...", ":", "/", "<",
                                   ">", "==", "!=", "&&", "||",
                                   ";", "++", "--", "+", "-",
                                   "*", "!"]
              , reservedNames = ["string", "int", "boolean"
                                 "if", "for", "else", "return"
                                 "false", "true", "import", "long"]
              }

{-Lexer-}
tParser = makeTokenParser def

mParens = parens tParser
mIdentifier = identifier tParser
mReservedOp = reservedOp tParser
mReserved = reserved tParser
mSemiSep1 = semiSep1 tParser
mWhiteSpace = whiteSpace tParser

{-Expression-}
