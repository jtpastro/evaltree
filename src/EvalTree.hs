{-# LANGUAGE FlexibleInstances #-}
module EvalTree where

-- An ExprTree is composed of branches Op and leaves Val
-- Val holds atomic values
-- Op holds a value, an expression and its left and right children of type ExprTree 
data ExprTree v e = Val v | Op v e (ExprTree v e) (ExprTree v e) deriving (Eq, Show)
data Step v e = LeftStep v e (ExprTree v e) | RightStep v e (ExprTree v e) deriving (Eq, Show)
type ExprZipper v e = (ExprTree v e, [Step v e])

-- An EvalTree is an instance of ExprTree
-- Val holds atomic values
-- Op holds a function, its children and the evaluation of its function with the value of its children
type EvalTree v = ExprTree v (v->v->v)
type EvalStep v = Step v (v->v->v)
type EvalZipper v =  (EvalTree v, [EvalStep v])

-- Making arithmetic operators comparable
instance (Eq a, Num a) => Eq (a-> a -> a) where 
  f == g = induce f g where
    base = 1
    n = 2
    induce f g = and [f 1 n' == g 1 n' | n' <- [base, n, n+1]]

-- Making arithmetic operators printable
instance (Eq a, Num a) => Show (a -> a -> a) where 
  show a = showOp a where
    showOp op = case lookup op ops of
                  Just a -> a
                  _  -> "undefined"
    ops = [((+),"+")
          ,((-),"-")
          ,((*),"*")]

-- initialize a zipper from an ExprTree
zipper :: ExprTree v e -> ExprZipper v e
zipper t = (t, [])

-- Walk left on the tree, store both the resulting tree and the untaken path, marked by its direction
goLeft :: ExprZipper v e -> ExprZipper v e
goLeft (Op v e l r, bs) = (l, LeftStep v e r:bs)
goLeft (Val x, bs) = (Val x, bs)

-- Walk right on the tree, store both the resulting tree and the untaken path, marked by its direction
goRight :: ExprZipper v e -> ExprZipper v e
goRight (Op v e l r, bs) = (r, RightStep v e l:bs)
goRight (Val v, bs) = (Val v, bs)

-- Take a step back restoring the tree structure
goUp :: ExprZipper v e -> ExprZipper v e
goUp (t, LeftStep v e r:bs) = (Op v e t r, bs)
goUp (t, RightStep v e l:bs) = (Op v e l t, bs)
goUp (t, []) =  (t, [])

-- Walk back to root restoring the original tree structure
topMost :: ExprZipper v e -> ExprZipper v e
topMost (t,[]) = (t,[])
topMost z = topMost $ goUp z

-- replace the current tree on focus with a given tree
replaceSubTree :: ExprTree v e -> ExprZipper v e -> ExprZipper v e 
replaceSubTree t (_, bs) = (t, bs)

-- replace Operation at root
replaceRootOp :: e -> ExprTree v e -> ExprTree v e
replaceRootOp e (Op v _ l r) = Op v e l r
replaceRootOp _ (Val v) = Val v

replaceOpAt :: e -> ExprZipper v e -> ExprZipper v e
replaceOpAt e (t, bs) = (replaceRootOp e t, bs)

-- Reverse function application
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- Retrieve the final value of an EvalTree
value ::  EvalTree v -> v
value (Val v) = v
value (Op v _ _ _) = v

-- reevaluate the root of an eval tree without reevaluating the whole tree
evalRoot ::  EvalTree v -> EvalTree v
evalRoot (Op _ f l r) = Op (f (value l) (value r)) f l r 
evalRoot (Val v) = Val v 

-- Go up on the tree until the root reevaluating only branches above the path taken
reEvalZipper ::  EvalZipper v  -> EvalZipper v
reEvalZipper (t,[]) = (evalRoot t,[])
reEvalZipper (t,bs) = reEvalZipper $ goUp (evalRoot t, bs)