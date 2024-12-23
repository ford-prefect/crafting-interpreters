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
  | False_
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | True_
  | Var
  | While
  | -- That's it
    Eof
  deriving (Eq, Show)

data Token = Token
  { tokenType :: TokenType,
    -- lexeme :: is embedded in the TokenType where needed
    -- literal :: is embedded in the TokenType where needed
    tokenLine :: Int
  }
  deriving (Eq, Show)
