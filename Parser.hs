module Parser where
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Abstract
import Control.Monad.Combinators.Expr
import Control.Monad (void)

import Text.Megaparsec.Debug

type Parser = Parsec Void String

run = runParser sourceParser

funParser :: Parser FunDef
funParser = do
  sc
  void $ symbol "fn"
  args <- parens $ sepBy (do
    t <- typeParser 
    sc
    a <- (do {(Var x) <- pVariable; return x})
    return (a,t))
    (symbol ",")
  sc
  rettype <- typeParser
  sc
  (Var name) <- pVariable
  void $ symbol ":"
  body <- manyTill (do {sc; statementParser}) (symbol "end")
  return (name,rettype,args,body)

sourceParser :: Parser FunEnv
sourceParser = many funParser

statementParser :: Parser Statement
statementParser = choice 
  [ do
    void (symbol "return")
    Return <$> expParser
  , try (do
      void (symbol "if")
      cond <- expParser
      void $ symbol "then"
      body <- manyTill statementParser (symbol "else")
      els  <- manyTill statementParser (symbol "end")
      return $ If cond body els)
    <|> 
      (do
        void (symbol "if")
        cond <- expParser
        void $ symbol "then"
        body <- manyTill statementParser (symbol "end")
        return $ If cond body [])
  , do
    void (symbol "print")
    Print <$> expParser
  , do
    void (symbol "input")
    (Var x) <- pVariable
    return $ Input x
  , do
    void (symbol "while")
    cond <- expParser
    void $ symbol "do"
    body <- manyTill statementParser (symbol "end")
    return $ While cond body
  , do
    ty <- typeParser
    sc
    (Var name) <- lookAhead pVariable
    return $ Declaration name (Const ty)
  , try (do
    (Var name) <- pVariable
    void (symbol "=")
    Assign name <$> expParser)
    <|> Exp <$> expParser
  ]

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ prefix "-" (PrimOp . Neg)
    , prefix "+" id
    , prefix "!" (PrimOp . BNot)
    ]
  , [ binary "*" (\x y -> PrimOp $ Mult x y)
    , binary "/" (\x y -> PrimOp $ Div x y)
    ]
  , [ binary "+" (\x y -> PrimOp $ Add x y)
    , binary "-" (\x y -> PrimOp $ Sub x y)
    ]
  , [ binary "=="  (\x y -> PrimOp $ Eq x y)]
  , [ binary "and" (\x y -> PrimOp $ BAnd x y)]
  , [ binary "or"  (\x y -> PrimOp $ BOr x y)]
  ]

binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expParser :: Parser Expression 
expParser = makeExprParser pTerm operatorTable 


pTerm :: Parser Expression
pTerm = choice
  [ parens expParser
  , Const <$> valParser
  , try (do 
      (Var name) <- pVariable
      args <- parens $ sepBy expParser (symbol ",")
      return $ Apply name args)
  <|> do 
    x <- pVariable 
    void $ notFollowedBy $ symbol "("
    return x]

sc :: Parser ()
sc = L.space 
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pInt :: Parser (Maybe Integer)
pInt = do 
  x <- L.signed sc $ lexeme L.decimal 
  return $ Just x

-- charLiteral :: Parser Char
-- charLiteral = between (char '\'') (char '\'') L.charLiteral

pString :: Parser (Maybe String)
pString = do
  s <- char '\"' *> manyTill L.charLiteral (char '\"')
  return $ Just s

pVariable :: Parser Expression 
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "proper variable name")

typeParser :: Parser Value
typeParser = choice
  [ BolV Nothing      <$ string "bool" <* notFollowedBy alphaNumChar 
  , IntV Nothing      <$ string "int" <* notFollowedBy alphaNumChar 
  , StrV Nothing      <$ string "string" <* notFollowedBy alphaNumChar
  , Void              <$ string "void" <* notFollowedBy alphaNumChar ]

valParser :: Parser Value
valParser = choice 
  [ BolV (Just True)  <$  symbol "true" 
  , BolV (Just False) <$  symbol "false"
  , IntV              <$> pInt
  , StrV              <$> pString ]
