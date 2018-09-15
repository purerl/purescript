module Language.PureScript.Parser.Erl (parseFile) where

import Prelude.Compat

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import qualified Text.Parsec.Char as PC

parseFile :: P.SourceName -> Text -> Either P.ParseError [(Text, Int)]
parseFile = P.parse parseLines

parseLines :: P.Parsec Text u [(Text, Int)]
parseLines = do
  l <- parseLine
  lns <- P.many $ do
    _ <- P.endOfLine
    parseLine
  P.eof
  pure (concat $ l : lns)

parseLine :: P.Parsec Text u [(Text, Int)]
parseLine = P.try parseAttribute <|>
  do
    P.skipMany (PC.noneOf ['\n', '\r'])
    pure []

parseAttribute :: P.Parsec Text u [(Text, Int)]
parseAttribute = attributeParser "export"
  (P.between (PC.char '[') (PC.char ']') (atomArityParser `P.sepBy` PC.char ','))

-- P.Parsec String u Token
--
attributeParser :: String -> P.Parsec Text u a -> P.Parsec Text u a
attributeParser name valueParser =
  -- PC.char '-' *> PC.string name *> P.between (PC.char '(') (PC.char ')') valueParser
  do
    _ <- PC.char '-'
    _ <- PC.string name
    res <- P.between (PC.char '(') (PC.char ')') valueParser
    _ <- PC.char '.'
    pure res

atomArityParser :: P.Parsec Text u (Text, Int)
atomArityParser = do
  PC.spaces
  a <- atomParser
  _ <- PC.char '/'
  n <- read <$> P.many1 PC.digit
  PC.spaces
  pure (a, n)

atomParser :: P.Parsec Text u Text
atomParser = quotedAtomParser <|> identifierParser

identifierParser :: P.Parsec Text u Text
identifierParser = do
  h <- PC.lower
  t <- T.pack <$> P.many (PC.alphaNum <|> PC.char '_' <|> PC.char '@')
  pure $ T.cons h t

quotedAtomParser :: P.Parsec Text u Text
quotedAtomParser = P.between (PC.char '\'') (PC.char '\'')
  (T.pack <$> P.many1 (PC.noneOf ['\'', '\\'] <|> atomEscapedCharParser))

atomEscapedCharParser :: P.Parsec Text u Char
atomEscapedCharParser = do
  _ <- PC.char '\\'
  PC.char '\'' <|> PC.char '\\'
