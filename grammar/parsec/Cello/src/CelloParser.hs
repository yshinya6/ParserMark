module CelloParser where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


{-setup reserved token-}
def = javaStyle{ identStart = letter <|> oneOf "_$"
              , identLetter = alphaNum <|> oneOf "_'$"
              , opStart = oneOf "=.:/%<>!^|&;+-*,?"
              , opLetter = oneOf "=.:/%<>^|&;+-*!,?"
              , reservedOpNames = ["=", "...", ":", "/", "%", "<",
                                   ">","<=", ">=", "==", "!=", "&&", "||",
                                   ";", "++", "--", "&", "+", "-",
                                   "*", "!", ",", "?", "*=", "/=",
                                  "%=", "+=", "-=", "<<=", ">>=", "::",
                                   ">>>=", "&=", "^=", "|="]
              , reservedNames = ["string", "int", "long", "boolean",
                                 "if", "for", "else", "return",
                                 "false", "true", "import", "null", "dummy1", "dummy2"]
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
exprparser' :: Parser String
exprparser' = buildExpressionParser table term <?> "expression"

table = [ [Prefix (celloReservedOp "!" >> return ((:) '!')) ]
        , [Infix (celloReservedOp "<" >> return (midleCons "<")) AssocLeft]
        , [Infix (celloReservedOp "<=" >> return (midleCons "<=")) AssocLeft]
        , [Infix (celloReservedOp ">" >> return (midleCons ">")) AssocLeft]
        , [Infix (celloReservedOp ">=" >> return (midleCons ">=")) AssocLeft]
        , [Infix (celloReservedOp "==" >> return (midleCons "==")) AssocLeft]
        , [Infix (celloReservedOp "!=" >> return (midleCons "!=")) AssocLeft]
        , [Infix (celloReservedOp "||" >> return (midleCons "||")) AssocLeft]
        , [Infix (celloReservedOp "&&" >> return (midleCons "&&")) AssocLeft]
        , [Infix (celloReservedOp "*=" >> return (midleCons "*=")) AssocLeft]
        , [Infix (celloReservedOp "/=" >> return (midleCons "/=")) AssocLeft]
        , [Infix (celloReservedOp "%=" >> return (midleCons "%=")) AssocLeft]
        , [Infix (celloReservedOp "+=" >> return (midleCons "+=")) AssocLeft]
        , [Infix (celloReservedOp "-=" >> return (midleCons "-=")) AssocLeft]
        , [Infix (celloReservedOp "<<=" >> return (midleCons "<<=")) AssocLeft]
        , [Infix (celloReservedOp ">>=" >> return (midleCons ">>=")) AssocLeft]
        , [Infix (celloReservedOp ">>>=" >> return (midleCons ">>>=")) AssocLeft]
        , [Infix (celloReservedOp "&=" >> return (midleCons "&=")) AssocLeft]
        , [Infix (celloReservedOp "^=" >> return (midleCons "^=")) AssocLeft]
        , [Infix (celloReservedOp "|=" >> return (midleCons "|=")) AssocLeft]
        , [Infix (celloReservedOp "=" >> return (midleCons "=")) AssocLeft]
        ]
        where
          midleCons :: String -> String -> String -> String
          midleCons op x y = x ++ op ++ y

term = try (celloParens exprparser)
    <|> try (stringLiteral tokenParser)
    <|> try functionExpression
    <|> try funccall
    <|> (celloIdentifier >>= (\n -> return n))
    <|> (celloReserved "true" >> return "true")
    <|> (celloReserved "false" >> return "false")
    <|> (celloReserved "null" >> return "null")
    <|> (integer tokenParser >>= return . show)

functionExpression :: Parser String
functionExpression = do
  typesParser
  ident <- optional celloIdentifier
  mpList <- celloParens fpListParser
  body <- blockParser
  celloReservedOp "::"
  return . concat $ [show ident, mpList, body]

exprparser :: Parser String
exprparser = try cond <|> exprparser'
    where
        cond = do
          c <- exprparser'
          celloReservedOp "?"
          t <- exprparser'
          celloReservedOp ":"
          e <- exprparser'
          return . concat $ [c, "?", t, ":" , e]

funccall :: Parser String
funccall = do
  funcname <- celloIdentifier
  args <- celloParens argExpList
  return $ funcname ++ (concat args)
  where
    argExpList = celloCommaSep exprparser

--Program Parser
programParser :: Parser String
programParser =  endBy topLevelParser eof >>= (return . concat)

topLevelParser :: Parser String
topLevelParser = do
  imdecls <- many importDeclarationparser
  decl <- many1 declarationParser
  return $ (concat (imdecls ++ decl))

importDeclarationparser :: Parser String
importDeclarationparser = do
  celloReserved "import"
  packageName <- concat <$> sepBy (celloIdentifier <|> string "*") (char '.')
  --endSymbol <- celloSemiColon
  return $ "import" ++ packageName-- ++ endSymbol

declarationParser :: Parser String
declarationParser = try functionDeclarationParser
                 <|> try variableDeclarationParser
                 <|> try dummyParser1
                 <|> dummyParser2
                 <?> "Declaration"

functionDeclarationParser :: Parser String
functionDeclarationParser = try (do
  typesParser
  name <- celloIdentifier
  fplist <- celloParens (optional fpListParser)
  block <- blockParser
  return $ concat [name, show fplist, block])
  <|> try (do
    typesParser
    name <- celloIdentifier
    fplist <- celloParens (optional fpListParser)
    endsim <- celloSemiColon
    return $ concat [name, show fplist, endsim])
  <|> do
    typesParser
    block <- blockParser
    return block
  <?> "functionDeclarationParser"

typesParser = celloReserved "string"
           <|> celloReserved "int"
           <|> celloReserved "long"
           <|> celloReserved "boolean"
           <?> "types"

fpListParser :: Parser String
fpListParser = do
  fp <- sepBy fParam (comma tokenParser)
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
stmtparser = try blockParser
          <|> try ifelsestmt
          <|> try ifstmt
          <|> try ifstmt1
          <|> try ifstmt2
          <|> try ifstmt3
          <|> try ifstmt4
          <|> try ifstmt5
          <|> try ifstmt6
          <|> try ifstmt7
          <|> try ifstmt8
          <|> try ifstmt9
          <|> try ifstmt10
          <|> try ifstmtA
          <|> try ifstmtB
          <|> try returnstmt
          <|> expstmt
            where
              ifstmt1 = ifstmt
              ifstmt2 = ifstmt
              ifstmt3 = ifstmt
              ifstmt4 = ifstmt
              ifstmt5 = ifstmt
              ifstmt6 = ifstmt
              ifstmt7 = ifstmt
              ifstmt8 = ifstmt
              ifstmt9 = ifstmt
              ifstmt10 = ifstmt
              ifstmtA = ifstmt
              ifstmtB = ifstmt
              ifelsestmt = do
                celloReserved "if"
                expr <- celloParens exprparser
                blc <- blockParser
                celloReserved "else"
                blc2 <- blockParser
                return $ concat ["if", expr, blc, "else", blc2]
              ifstmt = do -- if
                celloReserved "if"
                expr <- celloParens exprparser
                blc <- blockParser
                return $ concat ["if", expr, blc]
              returnstmt = do -- return
                celloReserved "return"
                expr <- optional (try exprparser <|> celloIdentifier)
                celloSemiColon
                return $ concat ["return",show expr, ";"]
              expstmt = do -- expStmt
                expr <- exprparser
                celloSemiColon
                return $ expr ++ ";"

dummyParser1 :: Parser String
dummyParser1 = do
  celloReserved "dummy1"
  variableList <- sepBy1 initDecl (comma tokenParser)
  celloSemiColon
  return $ "dummy1" ++ concat variableList ++ ";"

dummyParser2 :: Parser String
dummyParser2 = do
  celloReserved "dummy2"
  variableList <- sepBy1 initDecl (comma tokenParser)
  celloSemiColon
  return $ "dummy2" ++ concat variableList ++ ";"

variableDeclarationParser :: Parser String
variableDeclarationParser = do
  typesParser
  variableList <- sepBy1 initDecl (comma tokenParser)
  celloSemiColon
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
