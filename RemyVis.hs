-- simple visualization of Rémy's algorithm for generation of random binary trees

import Data.Maybe (fromJust)
import Data.Tree

import System.Random
import System.Environment

import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Layout.Tree

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

nodes :: Bin a -> Int
nodes (L _) = 0
nodes (B t1 t2) = 1 + nodes t1 + nodes t2

class Monad d => Sampling d where
  randR :: (Int,Int) -> d Int
  randB :: d Bool

instance Sampling IO where
  randR = getStdRandom . randomR
  randB = randomIO

graft :: Bin a -> a -> Bool -> Int -> (Bin a -> r) -> (Int -> r) -> r
graft t       x d 0 s f = s (if d then B (L x) t else B t (L x))
graft (B t u) x d n s f = graft t x d (n-1)
                          (\t' -> s (B t' u))
                          (\n' -> graft u x d n' (\u' -> s (B t u')) f)
graft (L _)   x d n s f = f (n-1)

grow :: Sampling d => a -> Bin a -> d (Bin a)
grow x t = do
  let n = nodes t
  i <- randR (0,2*n)
  d <- randB
  graft t x d i return undefined

remy :: Sampling d => [a] -> d (Bin a)
remy [x]    = return (L x)
remy (x:xs) = remy xs >>= grow x

remy' :: Sampling d => [a] -> d [Bin a]
remy' [x] = return [L x]
remy' (x:xs) = do
  (t:ts) <- remy' xs
  t' <- grow x t
  return (t':t:ts)

treeDiagram :: (Show a,Eq a) => a -> BTree (Maybe a) -> QDiagram Cairo V2 Double Any
treeDiagram x t =
  renderTree (nodeDiagram x) (~~)
    (addRoot $ fromJust $ symmLayoutBin' (with & slHSep .~ 1.5) t) # centerXY
  where
    nodeDiagram :: (Show a,Eq a) => a -> Maybe a -> QDiagram Cairo V2 Double Any
    nodeDiagram x Nothing  = mempty
    nodeDiagram x (Just y) = text (show y) # fc (if x == y then white else black) # scale 0.4 `atop`
                             circle 0.5 # fc (if x == y then green else lightgreen) # lw 0.2
    addRoot :: Num n => Tree (Maybe a, P2 n) -> Tree (Maybe a, P2 n)
    addRoot t = Node (Nothing, translateY 1 $ snd (rootLabel t)) [t]
    
remyAnim :: (Sampling d,Show a,Eq a) => [a] -> d [QDiagram Cairo V2 Double Any]
remyAnim xs = do
  ts <- remy' xs
  return $ drop 2 $ concatMap diags (reverse $ zip xs ts)
  where
    diags :: (Show a,Eq a) => (a,Bin a) -> [QDiagram Cairo V2 Double Any]
    diags (x,t) = map (treeDiagram x) [toBTree1 x t, toBTree2 x t, toBTree t]

    toBTree :: Bin a -> BTree (Maybe a)
    toBTree (L x)     = leaf (Just x)
    toBTree (B t1 t2) = BNode Nothing (toBTree t1) (toBTree t2)

    toBTree1 :: Eq a => a -> Bin a -> BTree (Maybe a)
    toBTree1 x (L y)
      | x == y           = Empty
      | otherwise        = leaf (Just y)
    toBTree1 x (B t1 t2) = BNode Nothing (toBTree1 x t1) (toBTree1 x t2)

    toBTree2 :: Eq a => a -> Bin a -> BTree (Maybe a)
    toBTree2 x (L y)
      | x == y           = leaf Nothing
      | otherwise        = leaf (Just y)
    toBTree2 x (B t1 t2) = BNode Nothing (toBTree2 x t1) (toBTree2 x t2)

main :: IO ()
main = do
  n <- pure read <*> getEnv "N"
  ds <- remyAnim [n,n-1..1]
  let d' = last ds
  gifMain [(withEnvelope d' d # pad 1.25 # bg white, 20) |
           d <- ds ++ reverse ds]

-- sample usage:
-- $ ghc -O2 RemyVis.hs
-- $ N=50 ./RemyVis -w 768 -o tree50.gif
