{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import qualified Data.Text as T
import LispVal
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Data.Char

-- newtype Parser LispVal = Parser (Text -> [(LispVal, Text)])

-- Lexer
lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style =
  Lang.emptyDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = "--",
      Tok.opStart = Tok.opLetter style,
      Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~",
      Tok.identStart = letter <|> oneOf "-+/*=|&><",
      Tok.identLetter = digit <|> letter <|> oneOf "?+|&-/",
      Tok.reservedOpNames = ["'", "\""]
    }

Tok.TokenParser
  { Tok.parens = m_parens,
    Tok.identifier = m_identifier
  } = Tok.makeTokenParser style

-- Parser
reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
  p <- m_identifier
  return $ Atom $ T.pack p

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf "\""
  reservedOp "\""
  return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
  char '-'
  d <- many1 digit
  return $ Number . negate . read $ d

parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp = List . concat <$> m_parens (Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved =
  do
    reservedOp "Nil" >> return Nil
    <|> (reservedOp "#t" >> return (Bool True))
    <|> (reservedOp "#f" >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr =
  parseReserved <|> parseNumber
    <|> try parseNegNum
    <|> parseAtom
    <|> parseText
    <|> parseQuote
    <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

lispVal :: Parser LispVal
lispVal = hashVal
 <|> Nil <$ nil
 <|> Number <$> try (sign <*> decimal)
 <|> Atom <$> identifier
 <|> String <$> textLiteral
 <|> _Quote <$> quoted lispVal
 <|> List <$> parens manyLispVal

hashVal :: Parser LispVal
hashVal = lexeme $ char '#'
  *> (char 't' *> return (Bool True)
  <|> char 'f' *> return (Bool False)
  <|> char 'b' *> (Number <$> intRadix (2, oneOf "01"))
  <|> char 'o' *> (Number <$> intRadix (8, octDigit))
  <|> char 'd' *> (Number <$> intRadix (10, digit))
  <|> char 'x' *> (Number <$> intRadix (16, hexDigit))
  <|> oneOf "ei" *> fail "Unsupported: exactness"
  <|> char '(' *> fail "Unsupported: vector"
  <|> char '\\' *> fail "Unsupported: char")

intRadix r = sign <*> numberWithRadix r
lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

type Radix = (Integer, Parser Char)

numberWithRadix :: Radix -> Parser Integer
numberWithRadix (base, baseDigit) = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

decimal :: Parser Integer
decimal = Tok.decimal lexer

sign :: Parser (Integer -> Integer)
sign = char '-' *> return negate
   <|> char '+' *> return id
   <|> return id

parens :: Parser a -> Parser a
parens = Tok.parens lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
    specialIdentifier = lexeme $ try $ string "-" <|> string "+" <|> string "..."

textLiteral :: Parser T.Text
textLiteral = T.pack <$> Tok.stringLiteral lexer

nil :: Parser ()
nil = try ((char '\'') *> string "()") *> return () <?> "nil"

_Quote :: LispVal -> LispVal
_Quote x = List [Atom "quote", x]

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` whitespace

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal))


