module Expr where
import EvalTree ( EvalTree, ExprTree(Op, Val), evalRoot )

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

instance Show Expr where
  show (Lit int) = show int
  show (Add e1 e2) = "(" <> show e1 <> " + " <> show e2 <> ")"
  show (Sub e1 e2) = "(" <> show e1 <> " - " <> show e2 <> ")"
  show (Mul e1 e2) = "(" <> show e1 <> " * " <> show e2 <> ")"

eval :: Expr -> EvalTree Int
eval (Lit i) = Val i
eval (Add e1 e2) = evalRoot $ Op 0 (+) (eval e1) (eval e2)
eval (Sub e1 e2) = evalRoot $ Op 0 (-) (eval e1) (eval e2)
eval (Mul e1 e2) = evalRoot $ Op 0 (*) (eval e1) (eval e2)
