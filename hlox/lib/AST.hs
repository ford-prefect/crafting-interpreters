module AST
  ( ASTVisitor,
    parse,
    prettyPrint,
  )
where

import Control.Applicative
import Data.Functor
import Parser
import Token qualified as T

data Expr
  = Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Unary UnaryExpr
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
  | Boolean Bool
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
  visitBinary :: BinaryExpr -> a
  visitGrouping :: GroupingExpr -> a
  visitLiteral :: LiteralExpr -> a
  visitUnary :: UnaryExpr -> a

runASTVisitor :: (ASTVisitor a) => Expr -> a
runASTVisitor (Binary e) = visitBinary e
runASTVisitor (Grouping e) = visitGrouping e
runASTVisitor (Literal e) = visitLiteral e
runASTVisitor (Unary e) = visitUnary e

instance ASTVisitor String where
  visitBinary (BinaryExpr left right op) =
    "(" ++ runASTVisitor left ++ " " ++ show op ++ " " ++ runASTVisitor right ++ ")"
  visitGrouping (GroupingExpr e) = "Group( " ++ runASTVisitor e ++ " )"
  visitLiteral (LiteralExpr t) = show t
  visitUnary (UnaryExpr expr op) = show op ++ " " ++ runASTVisitor expr

prettyPrint :: Expr -> String
prettyPrint = runASTVisitor

matchToken :: T.TokenType -> Parser [T.Token] T.Token
matchToken typ = satisfy (\t -> T.tokenType t == typ)

parseBinaryExpr :: T.TokenType -> BinaryOp -> Expr -> Parser [T.Token] Expr -> Parser [T.Token] Expr
parseBinaryExpr token op left parseRight =
  matchToken token *> (Binary <$> (BinaryExpr left <$> parseRight <*> pure op))

-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--               | "(" expression ")" ;
parsePrimary :: Parser [T.Token] Expr
parsePrimary = parseConstant <|> parseLiteral <|> parseGrouping
  where
    parseConstant =
      (matchToken T.False_ $> (Literal . LiteralExpr $ LitFalse))
        <|> (matchToken T.True_ $> (Literal . LiteralExpr $ LitTrue))
        <|> (matchToken T.Nil $> (Literal . LiteralExpr $ LitNil))
    parseLiteral = Parser $ \tokens -> case tokens of
      (T.Token (T.Num n) _ : rest) -> (Just . Literal . LiteralExpr . Number $ n, rest)
      (T.Token (T.Str s) _ : rest) -> (Just . Literal . LiteralExpr . String $ s, rest)
      _ -> (Nothing, tokens)
    parseGrouping = matchToken T.LeftParen *> parseExpression <* matchToken T.RightParen

-- unary          → ( "!" | "-" ) unary
--               | primary ;
parseUnary :: Parser [T.Token] Expr
parseUnary =
  matchToken T.Bang *> (Unary <$> (UnaryExpr <$> parseUnary <*> pure Not))
    <|> matchToken T.Minus *> (Unary <$> (UnaryExpr <$> parseUnary <*> pure Negate))
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
