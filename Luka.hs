-- routines for drawing Łukasiewicz paths

module Luka where

import Diagrams.Prelude hiding (E,normalize)
import Diagrams.Backend.SVG

import Data.List
import Data.Char

data Step = U Int | D
  deriving (Show,Eq)

type Walk = [Step]

readWalk :: String -> Walk
readWalk "" = []
readWalk ('D':w) = D : readWalk w
readWalk ('U':w) = U (read wn) : readWalk w'
  where
    (wn,w') = span isDigit w

stepV2 :: Step -> V2 Double
stepV2 (U n) = unitX + fromIntegral n * unitY
stepV2 D = unitX + unit_Y

qplane :: Double -> Double -> Diagram B
qplane dx dy =
  (atPoints [p2 (0,y) | y <- [1..dy]] (repeat $ fromSegments [straight (r2 (1+dx,0))] # lw 0.5 # dashingN [0.01,0.01] 0)) `atop`
  (atPoints [p2 (x,0) | x <- [1..dx]] (repeat $ fromSegments [straight (r2 (0,1+dy))] # lw 0.5 # dashingN [0.01,0.01] 0)) `atop`
  mconcat [arrowAt' arrowStyle origin ((1 + dx) *^ unitX), arrowAt' arrowStyle origin ((1 + dy) *^ unitY)] # lw 0.5
  where
    arrowStyle = (with & headLength .~ tiny)

walk :: Int -> Walk -> Diagram B
walk k p = strokeLine (lineFromOffsets (map stepV2 p)) # moveTo (p2 (0,fromIntegral k))

boundWalk :: Int -> Walk -> (Double,Double)
boundWalk k p = (dx,dy)
  where
    vs = scanl (+) (0 ^& fromIntegral k) (map stepV2 p)
    dx = maximum (map (fst . unr2) vs)
    dy = maximum (map (snd . unr2) vs)

-- example: walkSVG 1 (readWalk "U2DU1DDU1DD") "out"
walkSVG :: Int -> Walk -> String -> IO ()
walkSVG k p basename = do
  renderPretty (basename ++ ".svg") (mkWidth 1024) diag
  return ()
  where
    (dx,dy) = boundWalk k p
    diag = (walk k p `atop` qplane dx dy) # centerXY # pad 1.25

-- example: walkSVG' 1 "U2DU1DDU1DD"
walkSVG' :: Int -> String -> IO ()
walkSVG' k s = walkSVG k (readWalk s) s

walksSVG :: Int -> [(String,Walk)] -> String -> IO ()
walksSVG k nps basename = do
  renderPretty (basename ++ ".svg") (mkWidth (100 * dx * fromIntegral (length nps))) (hsep 1 diags)
  return ()
  where
    (ns,ps) = unzip nps
    bnds = map (boundWalk k) ps
    (dx,dy) = (maximum (map fst bnds), maximum (map snd bnds))
    diag (n,p) = vsep 0.5 [(walk k p `atop` qplane dx dy) # centerXY, text n # scale 0.4]  # pad 1.25
    diags = map diag nps
