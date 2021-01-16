-- visualizing conversion between trees and Łukasiewicz paths
-- uses visualization of operadic trees from the LinLam library

module LukaTree where

import Diagrams.Prelude hiding (E,normalize)
import Diagrams.Backend.SVG

import Diagrams.TwoD.Layout.Tree

import Luka

import LinLam.Diagrams.OpTree
import LinLam.OpTree

type Tree = OpTree () ()

-- generate all k-ary trees with n internal nodes
kTree :: Int -> Int -> [Tree]
kTree k n
  | n == 0 = [Leaf ()]
  | n > 0 = [Node () f | f <- kForest k (n-1) k]
-- generate all forests of k-ary trees with n internal nodes and p roots
kForest :: Int -> Int -> Int -> [[Tree]]
kForest k n p
  | p == 0 = [[] | n == 0]
  | otherwise = [t:f | m <- [0..n], t <- kTree k m, f <- kForest k (n-m) (p-1)]


-- conversion of k-ary trees + forests to Łukasiewicz paths

-- preorder left-to-right
preLR :: Tree -> Walk
preLR (Leaf _)   = [D 1]
preLR (Node _ f) = U (length f-1) : concatMap preLR f

-- preorder right-to-left
preRL :: Tree -> Walk
preRL (Leaf _)   = [D 1]
preRL (Node _ f) = U (length f-1) : concatMap preRL (reverse f)

-- postorder left-to-right
posLR :: Tree -> Walk
posLR (Leaf _)   = [U 1]
posLR (Node _ f) = concatMap posLR f ++ [D (length f-1)]

-- postorder right-to-left
posRL :: Tree -> Walk
posRL (Leaf _)   = [U 1]
posRL (Node _ f) = concatMap posRL (reverse f) ++ [D (length f-1)]

treeDiagram :: Tree -> Diagram B
treeDiagram t = fst $ diagTree (Spec { ld = \_ -> circle 0.1 # fc lightblue , nd = \_ -> circle 0.1 # fc red , rd = mempty }) "" t

treeWalksDiagram :: Tree -> Diagram B
treeWalksDiagram t = hsep 1 [treeDiagram t # centerXY,
                             gridWalk 1 (preLR t) # centerXY # pad 1.1,
                             gridWalk 1 (preRL t) # centerXY # pad 1.1,
                             gridWalk 0 (posLR t) # centerXY # pad 1.1,
                             gridWalk 0 (posRL t) # centerXY # pad 1.1
                             ] # centerXY # pad 1.1


-- binary and ternary trees
lea = Leaf ()
bin x y = Node () [x,y]
ter x y z = Node () [x,y,z]
