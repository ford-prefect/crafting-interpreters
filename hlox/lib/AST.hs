module AST
  ( ASTVisitor,
    parse,
    prettyPrint,
  )
where

import Control.Applicative
import Parser
import Token qualified as T

data Expr
  = Binary T.Token BinaryExpr
  | Grouping T.Token GroupingExpr
  | Literal T.Token LiteralExpr
  | Unary T.Token UnaryExpr
  deriving (Show)

data EqualityOp
  = IsEqual
  | NotEqual
  deriving (Show)

data ComparisonOp
  = Greater
  | GreaterEqual
  | Less
  | LessEqual
  deriving (Show)

data ArithmeticOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data BinaryOp
  = Equality EqualityOp
  | Comparison ComparisonOp
  | Arithmetic ArithmeticOp
  deriving (Show)

data UnaryOp
  = Negate
  | Not
  deriving (Show)

data Literal
  = Number Double
  | String String
  | LitTrue
  | LitFalse
  | LitNil
  deriving (Show)

data BinaryExpr = BinaryExpr
  { binaryExprLeft :: Expr,
    binaryExprRight :: Expr,
    binaryExprOp :: BinaryOp
  }
  deriving (Show)

newtype GroupingExpr = GroupingExpr Expr
  deriving (Show)

newtype LiteralExpr = LiteralExpr Literal
  deriving (Show)

data UnaryExpr = UnaryExpr
  { unaryExprExp :: Expr,
    unaryExprOp :: UnaryOp
  }
  deriving (Show)

class ASTVisitor a where
  visitBinary :: T.Token -> BinaryExpr -> a
  visitGrouping :: T.Token -> GroupingExpr -> a
  visitLiteral :: T.Token -> LiteralExpr -> a
  visitUnary :: T.Token -> UnaryExpr -> a

runASTVisitor :: (ASTVisitor a) => Expr -> a
runASTVisitor (Binary t e) = visitBinary t e
runASTVisitor (Grouping t e) = visitGrouping t e
runASTVisitor (Literal t e) = visitLiteral t e
runASTVisitor (Unary t e) = visitUnary t e

instance ASTVisitor String where
  visitBinary _ (BinaryExpr left right op) =
    "(" ++ runASTVisitor left ++ " " ++ show op ++ " " ++ runASTVisitor right ++ ")"
  visitGrouping _ (GroupingExpr e) = "Group( " ++ runASTVisitor e ++ " )"
  visitLiteral _ (LiteralExpr t) = show t
  visitUnary _ (UnaryExpr expr op) = show op ++ " " ++ runASTVisitor expr

prettyPrint :: Expr -> String
prettyPrint = runASTVisitor

matchToken :: T.TokenType -> Parser [T.Token] T.Token
matchToken typ = satisfy (\t -> T.tokenType t == typ)

parseBinaryExpr :: T.TokenType -> BinaryOp -> Expr -> Parser [T.Token] Expr -> Parser [T.Token] Expr
parseBinaryExpr token op left parseRight =
  matchToken token >>= (\t -> Binary t <$> (BinaryExpr left <$> parseRight <*> pure op))

-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--               | "(" expression ")" ;
parsePrimary :: Parser [T.Token] Expr
parsePrimary = parseConstant <|> parseLiteral <|> parseGrouping
  where
    parseConstant =
      (matchToken T.False_ >>= (\t -> pure . Literal t . LiteralExpr $ LitFalse))
        <|> (matchToken T.True_ >>= (\t -> pure . Literal t . LiteralExpr $ LitTrue))
        <|> (matchToken T.Nil >>= (\t -> pure . Literal t . LiteralExpr $ LitNil))
    parseLiteral = Parser $ \tokens -> case tokens of
      (t@(T.Token (T.Num n) _) : rest) -> (Just . Literal t . LiteralExpr . Number $ n, rest)
      (t@(T.Token (T.Str s) _) : rest) -> (Just . Literal t . LiteralExpr . String $ s, rest)
      _ -> (Nothing, tokens)
    parseGrouping = matchToken T.LeftParen *> parseExpression <* matchToken T.RightParen

-- unary          → ( "!" | "-" ) unary
--               | primary ;
parseUnary :: Parser [T.Token] Expr
parseUnary =
  (matchToken T.Bang >>= (\t -> Unary t <$> (UnaryExpr <$> parseUnary <*> pure Not)))
    <|> (matchToken T.Minus >>= (\t -> Unary t <$> (UnaryExpr <$> parseUnary <*> pure Negate)))
    <|> parsePrimary

-- factor         → unary ( ( "/" | "*" ) unary )* ;
parseFactor :: Parser [T.Token] Expr
parseFactor = parseUnary >>= parseFactor'
  where
    parseFactor' :: Expr -> Parser [T.Token] Expr
    parseFactor' e =
      parseBinaryExpr T.Slash (Arithmetic Divide) e parseFactor
        <|> parseBinaryExpr T.Star (Arithmetic Multiply) e parseFactor
        <|> pure e

-- term           → factor ( ( "-" | "+" ) factor )* ;
parseTerm :: Parser [T.Token] Expr
parseTerm = parseFactor >>= parseTerm'
  where
    -- for the ( ( "-" | "+" ) factor )* part
    parseTerm' :: Expr -> Parser [T.Token] Expr
    parseTerm' e =
      parseBinaryExpr T.Minus (Arithmetic Subtract) e parseTerm
        <|> parseBinaryExpr T.Plus (Arithmetic Add) e parseTerm
        <|> pure e

-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
parseComparison :: Parser [T.Token] Expr
parseComparison = parseTerm >>= parseComparison'
  where
    -- for the ( ( ">" | ">=" | "<" | "<=" ) term )* part
    parseComparison' :: Expr -> Parser [T.Token] Expr
    parseComparison' e =
      parseBinaryExpr T.Greater (Comparison Greater) e parseComparison
        <|> parseBinaryExpr T.GreaterEqual (Comparison GreaterEqual) e parseComparison
        <|> parseBinaryExpr T.Less (Comparison Less) e parseComparison
        <|> parseBinaryExpr T.LessEqual (Comparison LessEqual) e parseComparison
        <|> pure e

-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
parseEquality :: Parser [T.Token] Expr
parseEquality = parseComparison >>= parseEquality'
  where
    -- for the ( ( "!=" | "==" ) comparison )* part
    parseEquality' :: Expr -> Parser [T.Token] Expr
    parseEquality' e =
      parseBinaryExpr T.BangEqual (Equality NotEqual) e parseEquality
        <|> parseBinaryExpr T.Equal (Equality IsEqual) e parseEquality
        <|> pure e

-- expression     → equality ;
parseExpression :: Parser [T.Token] Expr
parseExpression = parseEquality

parse :: [T.Token] -> (Maybe Expr, Maybe T.Token)
parse tokens = case runParser parseEquality tokens of
  (Just e, [T.Token T.Eof _]) -> (Just e, Nothing)
  (me, []) -> (me, Nothing)
  (me, t) -> (me, Just . head $ t)
