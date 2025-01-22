module AST where

import Token (Token)

data Expr
  = Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Unary UnaryExpr

-- FIXME: Can we narrow tokens below to only valid tokens?

data BinaryExpr = BinaryExpr
  { binaryExprLeft :: Expr,
    binaryExprRight :: Expr,
    binaryExprOp :: Token
  }

newtype GroupingExpr = GroupingExpr Expr

newtype LiteralExpr = LiteralExpr Token

data UnaryExpr = UnaryExpr
  { unaryExprExp :: Expr,
    unaryExprOp :: Token
  }

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
