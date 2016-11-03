module CelloParser where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


{-setup reserved token-}
def = javaStyle{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "=.:/%<>!^|&;+-*"
              , opLetter = oneOf "=.:/%<>^|&;+-*!"
              , reservedOpNames = ["=", "...", ":", "/", "%", "<",
                                   ">", "==", "!=", "&&", "||",
                                   ";", "++", "--", "&", "+", "-",
                                   "*", "!"]
              , reservedNames = ["string", "int", "long", "boolean",
                                 "if", "for", "else", "return",
                                 "false", "true", "import", "null"]
              }

{-Lexer-}
tokenParser = makeTokenParser def

{-parser units for Cello-}
celloParens = parens tokenParser
celloBraces = braces tokenParser
celloIdentifier = identifier tokenParser
celloReservedOp = reservedOp tokenParser
celloReserved = reserved tokenParser
celloSemiSep1 = semiSep1 tokenParser
celloWhiteSpace = whiteSpace tokenParser
celloCommaSep = commaSep tokenParser
celloSemiColon = semi tokenParser

{-expression Parser-}
exprparser :: Parser String
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (do
                    funcname <- celloIdentifier
                    return ((++) funcname)) ]
        , [Prefix (celloReservedOp "!" >> return ((:) '!')) ]
        , [Infix (celloReservedOp "<" >> return (midleCons "<")) AssocLeft]
        , [Infix (celloReservedOp "==" >> return (midleCons "==")) AssocLeft]
        , [Infix (celloReservedOp "!=" >> return (midleCons "!=")) AssocLeft]
        , [Infix (celloReservedOp "&&" >> return (midleCons "&&")) AssocLeft]
        , [Infix (celloReservedOp "=" >> return (midleCons "=")) AssocLeft]
        ]
        where
          midleCons :: String -> String -> String -> String
          midleCons op x y = x ++ op ++ y

term = celloParens exprparser
    <|> (celloIdentifier >>= (\n -> return n))
    <|> (celloReserved "true" >> return "true")
    <|> (celloReserved "false" >> return "false")

{-Program Parser-}
programParser :: Parser String
programParser =  endBy topLevelParser eof >>= (return . concat)

topLevelParser :: Parser String
topLevelParser = do
  imdecls <- many importDeclarationparser
  decl <- declarationParser
  return $ (concat imdecls) ++ decl

importDeclarationparser :: Parser String
importDeclarationparser = do
  celloReserved "import"
  packageName <- concat <$> sepBy (celloIdentifier <|> string "*") (char '.')
  endSymbol <- celloSemiColon
  return $ "import" ++ packageName ++ endSymbol

declarationParser :: Parser String
declarationParser = (functionDeclarationParser <|> variableDeclarationParser)

functionDeclarationParser :: Parser String
functionDeclarationParser = do
  typesParser
  name <- celloIdentifier
  fplist <- celloParens (optional fpListParser)
  block <- blockParser
  return $ concat [name, show fplist, block]
  <|> do
    typesParser
    name <- celloIdentifier
    fplist <- celloParens (optional fpListParser)
    endsim <- celloSemiColon
    return $ concat [name, show fplist, endsim]
  <|> do
    typesParser
    block <- blockParser
    return block

typesParser = celloReserved "string"
           <|> celloReserved "int"
           <|> celloReserved "long"
           <|> celloReserved "boolean"
           <?> "types"

fpListParser :: Parser String
fpListParser = do
  fp <- sepBy fParam (char ',')
  return $ concat fp
                where
                  fParam = do
                    typesParser
                    name <- optional celloIdentifier
                    return $ "types" ++ (show name)

blockParser :: Parser String
blockParser = do
  innar <- celloBraces (many blockinnar)
  return $ concat $ concat innar
                where
                  blockinnar = return <$> (stmtparser <|> declarationParser)

stmtparser :: Parser String
stmtparser = blockParser
          <|> ifstmt
          <|> returnstmt
          <|> expstmt
            where
              ifstmt = do -- if
                celloReserved "if"
                expr <- celloParens (exprparser)
                blc <- blockParser
                return $ concat ["if", expr, blc]
              returnstmt = do -- return
                celloReserved "return"
                expr <- optional exprparser
                char ';'
                return $ concat ["if",show expr, ";"]
              expstmt = do -- expStmt
                expr <- exprparser
                char ';'
                return $ expr ++ ";"

variableDeclarationParser :: Parser String
variableDeclarationParser = do
  typesParser
  variableList <- sepBy1 initDecl (char ',')
  char ';'
  return $ concat variableList ++ ";"

initDecl :: Parser String
initDecl = do
  name <- celloIdentifier
  initialer <- optional initializer
  return $ name ++ show initialer
  where
    initializer = do
      celloReservedOp "="
      ini <- exprparser
      return $ '=':ini
