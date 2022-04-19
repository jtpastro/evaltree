module Main (main) where
import EvalTree

-- Testing
-- Base tree
tree1 = Expr 6 (*) 
               (Expr 2 (+)
                       (Val 1) (Val 1)) 
               (Expr 3 (+)
                       (Val 1) (Val 2))
-- base zipper
zipper1 = zipper tree1

-- zipper from walking  left on the tree and its resulting tree
zipper2 =  zipper1 -: goRight -: goLeft

-- zipper and resulting tree from replacing a node on zipper2
zipper3 = zipper2 -: replaceSubTree (Val 3)
tree3 = zipper3 -: topMost -: fst

-- resulting tree from reevaluating zipper2
tree4 = zipper3 -: reEvalZipper -: fst


main :: IO ()
main = do
    print tree1
    print zipper2
    print tree3
    print tree4
