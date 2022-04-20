module Main (main) where
import EvalTree

import Expr

-- Testing

-- Base arithmetic expression
expr1 = Mul (Add (Lit 1) (Lit 1)) (Add (Lit 1) (Lit 2))
-- Base tree
tree1 = eval expr1
--- tree1 = Op 6 (*) 
---              (Op 2 (+)
---                    (Val 1) (Val 1)) 
---              (Op 3 (+)
---                    (Val 1) (Val 2))

-- base zipper
zipper1 = zipper tree1

-- zipper from walking  left on the tree and its resulting tree
zipper2 =  zipper1 -: goRight -: goLeft

-- zipper and resulting tree from replacing a node on zipper2
expr2 = Sub (Lit 4) (Lit 1)
zipper3 = zipper2 -: replaceSubTree (eval expr2)
tree3 = zipper3 -: topMost -: fst

-- resulting tree from reevaluating zipper2
tree4 = zipper3 -: reEvalZipper -: fst

-- replace op at root of tree1
tree5 = tree1 -: replaceRootOp (-) -: evalRoot

-- replace op at root of tree1
tree6 = zipper2 -: goUp -: replaceOpAt (-) -: reEvalZipper -: fst

main :: IO ()
main = do
    print expr1
    print tree1
    print expr2
    print zipper2
    print tree3
    print tree4
    print tree5
    print tree6