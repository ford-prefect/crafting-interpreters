module AST where

data Expr
  = Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Unary UnaryExpr
  deriving (Show)

data BinaryOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data UnaryOp
  = Negate
  | Not
  deriving (Show)

data Literal
  = Number Double
  | String String
  | Boolean Bool
  | Nil
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

runASTVisitor :: ASTVisitor a => Expr -> a
runASTVisitor (Binary e) = visitBinary e
runASTVisitor (Grouping e) = visitGrouping e
runASTVisitor (Literal e) = visitLiteral e
runASTVisitor (Unary e) = visitUnary e

instance ASTVisitor String where
  visitBinary (BinaryExpr left right op) =
    runASTVisitor left ++ " " ++ show op ++ " " ++ runASTVisitor right
  visitGrouping (GroupingExpr e) = "Group( " ++ runASTVisitor e ++ " )"
  visitLiteral (LiteralExpr t) = show t
  visitUnary (UnaryExpr expr op) = show op ++ " " ++ runASTVisitor expr

prettyPrint :: Expr -> String
prettyPrint = runASTVisitor
