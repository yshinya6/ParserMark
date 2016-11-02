module CelloParser where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


{-setup reserved token-}
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "=.:/%<>!^|&;+-*"
              , opLetter = oneOf "=.:/%<>^|&;+-*!"
              , reservedOpNames = ["=", "...", ":", "/", "%", "<",
                                   ">", "==", "!=", "&&", "||",
                                   ";", "++", "--", "&", "+", "-",
                                   "*", "!"]
              , reservedNames = ["string", "int", "long", "boolean"
                                 "if", "for", "else", "return"
                                 "false", "true", "import", "null"]
              }

{-Lexer-}
tokenParser = makeTokenParser def

{-parser units for Cello-}
celloParens = parens tokenParser
celloIdentifier = identifier tokenParser
celloReservedOp = reservedOp tokenParser
celloReserved = reserved tokenParser
celloSemiSep1 = semiSep1 tokenParser
celloWhiteSpace = whiteSpace tokenParser
celloCommaSep = commaSep tokenParser
celloSemiColon = semi tokenParser

{-Cello Parser in Nez Like Fassion-}
celloFile :: Parser String
celloFile = celloWhiteSpace >> celloImportDecl >> many celloImportDecl >> many (try celloToplevel <|> (celloWhiteSpace >> return []))

celloImportDecl :: Parser String
celloImportDecl = celloReserved "import" >> celloIdentifier >> many (char '.' >> celloIdentifier)

celloToplevel :: Parser String
celloToplevel = try celloDecl
             <|> string ";"

celloDecl :: Parser String
celloDecl = try celloMeathodDecl
         <|> celloVarDecl

celloMeathodDecl :: Parser String
celloMeathodDecl = celloType >> celloWhiteSpace >> celloIdentifier >> celloParens cellMPList >> celloWhiteSpace
                 >> (try celloBlock <|> string ";")

celloType = try celloPremitiveType <|> celloRefType <?> "type"

celloPremitiveType = celloReserved "string"
                  <|> celloReserved "int"
                  <|> celloReserved "long"
                  <|> celloReserved "boolean"

celloRefType = celloIdentifier

cellMPList =  optional (celloCommaSep cellMP) >> optional (string ",..." )
  where
    cellMP = celloType >> celloWhiteSpace >> optional celloIdentifier

celloBlock = celloIdentifier -- fixme

celloVarDecl =  celloType >> celloWhiteSpace >> valList >> celloSemiColon
  where
    valList = celloCommaSep iniDecl
    iniDecl = celloIdentifier -- fixme
