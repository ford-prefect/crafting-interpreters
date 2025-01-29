{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative

newtype Parser i o = Parser {runParser :: i -> (Maybe o, i)}

instance Functor (Parser i) where
  -- Apply a function to the output of the parser
  fmap f p = Parser $ \i -> case runParser p i of
    (Nothing, i') -> (Nothing, i')
    (Just o, i') -> (Just (f o), i')

instance Applicative (Parser i) where
  -- Lift a value and always return it
  pure output = Parser (Just output,)

  -- Apply the first parser, get a function to apply to the output of the second parser
  pf <*> po = Parser $ \input ->
    case runParser pf input of
      (Nothing, _) -> (Nothing, input)
      (Just f, rest) -> case runParser po rest of
        (Nothing, _) -> (Nothing, input)
        (Just o, rest') -> (Just (f o), rest')

instance Monad (Parser i) where
  (>>=) :: Parser i o -> (o -> Parser i o') -> Parser i o'
  (>>=) p f = Parser $ \input ->
    case runParser p input of
      (Nothing, _) -> (Nothing, input)
      (Just o, rest) -> runParser (f o) rest

instance Alternative (Parser i) where
  -- Parse nothing, return the input as is
  empty = Parser (Nothing,)

  -- Try the first parser, if it fails, try the second parser
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      (Nothing, _) -> runParser p2 input
      (Just o, rest) -> (Just o, rest)

satisfy :: (i -> Bool) -> Parser [i] i
satisfy p = Parser $ \input ->
  case input of
    (i : is) | p i -> (Just i, is)
    _ -> (Nothing, input)

char :: Char -> Parser String Char
char c = satisfy (== c)

word :: String -> Parser String String
word "" = pure ""
word (c:cs) = (:) <$> char c <*> word cs

digit :: Parser String Char
digit = satisfy (`elem` ['0' .. '9'])
