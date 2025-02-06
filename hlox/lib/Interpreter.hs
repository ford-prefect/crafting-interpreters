{-# LANGUAGE LambdaCase #-}

module Interpreter
  ( Error (..),
    Value (..),
    interpret,
  )
where

import AST
import qualified Token as T

data Error = Error
  { errorToken :: T.Token
  , errorMessage :: String
  }
  deriving (Show)

data Value
  = NilValue
  | BoolValue Bool
  | NumberValue Double
  | StringValue String
  deriving (Show, Eq, Ord)

evaluateUnary :: Expr -> UnaryOp -> Either String Value
evaluateUnary e op = case op of
  Negate -> runASTVisitor e >>= \case
    NumberValue n -> Right $ NumberValue (-n)
    _ -> Left $ "negated a non-number: " ++ show e
  Not -> runASTVisitor e >>= \x -> Right $ case x of
    BoolValue True -> BoolValue False
    BoolValue False -> BoolValue True
    NilValue -> BoolValue False
    _ -> BoolValue True

evaluateArithmetic :: Expr -> Expr -> ArithmeticOp -> Either String Value
evaluateArithmetic l r op =
  let lv = runASTVisitor l
      rv = runASTVisitor r
   in
      (,) <$> lv <*> rv >>= \case
        (NumberValue ln, NumberValue lr) -> Right $ case op of
          Add -> NumberValue $ ln + lr
          Subtract -> NumberValue $ ln - lr
          Multiply -> NumberValue $ ln * lr
          Divide -> NumberValue $ ln / lr
        _ -> Left $ "arithmetic on non-numbers: " ++ show lv ++ " " ++ show op ++ " " ++ show rv

evaluateEquality :: Expr -> Expr -> EqualityOp -> Either String Value
evaluateEquality l r op =
  let
      lv :: Either String Value
      lv = runASTVisitor l
      rv :: Either String Value
      rv = runASTVisitor r
   in
      case op of
        IsEqual -> BoolValue <$> ((==) <$> lv <*> rv)
        NotEqual -> BoolValue <$> ((/=) <$> lv <*> rv)

evaluateComparison :: Expr -> Expr -> ComparisonOp -> Either String Value
evaluateComparison l r op =
  let
      lv :: Either String Value
      lv = runASTVisitor l
      rv :: Either String Value
      rv = runASTVisitor r
   in
      case op of
        Greater -> BoolValue <$> ((>) <$> lv <*> rv)
        GreaterEqual -> BoolValue <$> ((>=) <$> lv <*> rv)
        Less -> BoolValue <$> ((<) <$> lv <*> rv)
        LessEqual -> BoolValue <$> ((<=) <$> lv <*> rv)

evaluateBinary :: Expr -> Expr -> BinaryOp -> Either String Value
evaluateBinary l r (Arithmetic op) = evaluateArithmetic l r op
evaluateBinary l r (Equality op) = evaluateEquality l r op
evaluateBinary l r (Comparison op) = evaluateComparison l r op

evaluateLiteral :: Literal -> Value
evaluateLiteral LitNil = NilValue
evaluateLiteral LitTrue = BoolValue True
evaluateLiteral LitFalse = BoolValue False
evaluateLiteral (Number n) = NumberValue n
evaluateLiteral (String s) = StringValue s

instance ASTVisitor (Either String Value) where
  visitGrouping _ (GroupingExpr e) = runASTVisitor e
  visitUnary _ (UnaryExpr e op) = evaluateUnary e op
  visitBinary _ (BinaryExpr l r op) = evaluateBinary l r op
  visitLiteral _ (LiteralExpr e) = Right $ evaluateLiteral e

wrapError :: T.Token -> Either String Value -> Either Error Value
wrapError t (Left msg) = Left $ Error t msg
wrapError _ (Right val) = Right val

instance ASTVisitor (Either Error Value) where
  visitGrouping t (GroupingExpr e) = wrapError t $ runASTVisitor e
  visitUnary t (UnaryExpr e op) = wrapError t $ evaluateUnary e op
  visitBinary t (BinaryExpr l r op) = wrapError t $ evaluateBinary l r op
  visitLiteral _ (LiteralExpr e) = Right $ evaluateLiteral e

interpret :: Expr -> Either Error Value
interpret = runASTVisitor
