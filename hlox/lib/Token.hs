module Token where

data TokenType
  = -- Single-character tokens
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | -- One-or-two-character tokens
    Bang
  | BangEqual
  | Equal
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | -- Literals
    Identifier String
  | Str String
  | Num Double
  | -- Keywords
    And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | True
  | Var
  | While
  | -- That's it
    Eof

data Token = Token
  { tokenType :: TokenType,
    tokenLexeme :: String,
    -- literal :: is embedded in the TokenType
    tokenLine :: Int
  }
