module Scanner
  ( Error (..),
    scan,
  )
where

import Control.Applicative (Alternative (many, some), (<|>))
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (($>))
import Parser
import Token

data Error = Error
  { errorLine :: Int,
    errorMessage :: String
  }
  deriving (Show)

singleCharTokens :: [(Char, TokenType)]
singleCharTokens =
  [ ('(', LeftParen),
    (')', RightParen),
    ('{', LeftBrace),
    ('}', RightBrace),
    (',', Comma),
    ('.', Dot),
    ('-', Minus),
    ('+', Plus),
    (';', Semicolon),
    ('/', Slash),
    ('*', Star),
    ('!', Bang),
    ('=', Equal),
    ('>', Greater),
    ('<', Less)
  ]

twoCharTokens :: [(String, TokenType)]
twoCharTokens =
  [ ("!=", BangEqual),
    ("==", Equal),
    (">=", GreaterEqual),
    ("<=", LessEqual)
  ]

keywords :: [(String, TokenType)]
keywords =
  [ ("and", And),
    ("class", Class),
    ("else", Else),
    ("false", False_),
    ("fun", Fun),
    ("for", For),
    ("if", If),
    ("nil", Nil),
    ("or", Or),
    ("print", Print),
    ("return", Return),
    ("super", Super),
    ("true", True_),
    ("var", Var),
    ("while", While)
  ]

scanSingleChar :: Int -> Parser String Token
scanSingleChar line =
  Parser $ \input ->
    case input of
      (c : cs) ->
        case lookup c singleCharTokens of
          Just t -> (Just (Token t line), cs)
          Nothing -> (Nothing, input)
      _ -> (Nothing, input)

scanMultiChar :: [(String, TokenType)] -> Int -> Parser String Token
scanMultiChar set line =
  Parser $ \input ->
    case lookup input set of
      Just t -> (Just (Token t line), "")
      Nothing -> (Nothing, input)

scanTwoChars :: Int -> Parser String Token
scanTwoChars = scanMultiChar twoCharTokens

scanKeywords :: Int -> Parser String Token
scanKeywords = scanMultiChar keywords

scanComment :: Parser String ()
scanComment =
  (word "//" *> many (satisfy (/= '\n')) *> char '\n') $> ()

scanString :: Int -> Parser String Token
scanString line =
  char '"' *> (Token <$> (Str <$> many (satisfy (/= '"')) <* char '"') <*> pure line)

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\r' || c == '\t'

scanSpace :: Parser String ()
scanSpace = satisfy isSpace $> ()

scanNumber :: Int -> Parser String Token
scanNumber line =
  let numberWithoutDecimal = some digit
      numberWithDecimal =
        ( (++)
            -- Take some digits ...
            <$> some digit
            -- ... then a dot + some digits, else produce an empty string for the outer '++'
            <*> (((:) <$> char '.' <*> some digit) <|> pure "")
        )
   in (Token . Num . read <$> (numberWithDecimal <|> numberWithoutDecimal) <*> pure line)

scanIdentifier :: Int -> Parser String Token
scanIdentifier line = Token . Identifier <$> ((:) <$> satisfy isIdPrefix <*> many (satisfy isIdChar)) <*> pure line
  where
    isIdPrefix c = isAlpha c || c == '_'
    isIdChar c = isAlphaNum c || c == '_'

scanToken :: Int -> Parser String Token
scanToken line =
  scanKeywords line <|> scanIdentifier line <|> scanNumber line <|> scanString line <|> scanTwoChars line <|> scanSingleChar line

scan' :: Int -> String -> (Maybe Error, [Token])
scan' line "" = (Nothing, [Token Eof line])
scan' line ('\n' : rest) = scan' (line + 1) rest
scan' line s@(c : _)
  | isSpace c =
      let rest = snd $ runParser (some scanSpace) s
       in scan' line rest
scan' line s = case runParser scanComment s of
  (Just _, rest') -> scan' (line + 1) rest'
  (Nothing, _) -> case runParser (scanToken line) s of
    (Just t, rest') -> (t :) <$> scan' line rest'
    (Nothing, _) -> (Just $ Error line ("Unexpected character" ++ [head s]), [])

scan :: String -> (Maybe Error, [Token])
scan = scan' 0
