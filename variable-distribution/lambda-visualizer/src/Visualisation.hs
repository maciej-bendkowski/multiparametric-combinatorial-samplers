-- | Lambda term sampler sitting on top of Sampler.hs
-- | Note: The package is meant for testing purposes only.
-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Visualisation
    ( toDotRepresentation
    ) where

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as B

import Data.Map (Map)
import qualified Data.Map as M

import System.Random

import qualified Lambda as L

data RLambda = RIndex Int Int
             | RAbs RLambda Int
             | RApp RLambda RLambda Int

-- | Gets the label of the given ranked λ-term.
label :: RLambda -> Int
label (RIndex _ r) = r
label (RAbs _ r)   = r
label (RApp _ _ r) = r

-- | Ranks the λ-term with unique integer
--   labels starting with the given number r.
rankL :: Int -> L.Lambda -> (RLambda, Int)
rankL r (L.Index n) = (RIndex (L.value n) r, r)
rankL r (L.Abs t) = case rankL (r+1) t of
    (rt, r') -> (RAbs rt r, r')
rankL r (L.App t t') = case rankL (r+1) t of
    (rt, r') -> case rankL (r'+1) t' of
        (rt', r'') -> (RApp rt rt' r, r'')

-- | Single point node.
pointNode :: Map Int Color -> Int -> Int -> DotNode String
pointNode f n idx = DotNode (show n) [textLabel (B.pack $ show idx),
                                      degColor f idx]

-- | Single labeled node.
labelNode :: Int -> String -> X11Color -> DotNode String
labelNode n s c = DotNode (show n) [textLabel (B.pack s)
                                   ,fillColor c]

-- | Constructs a list of graph nodes based on the given ranked λ-term.
nodes :: Map Int Color -> RLambda -> [DotNode String]
nodes f (RIndex n r)  = [pointNode f r n]
nodes f (RAbs t r)    = labelNode r "λ" Azure  : nodes f t
nodes f (RApp t t' r) = labelNode r "@" Gold : nodes f t ++ nodes f t'

-- | Single point edge.
pointEdge :: Int -> Int -> DotEdge String
pointEdge n k = DotEdge (show n) (show k) []

-- | Constructs a list of graph edges
--   based on the given ranked λ-term.
edges :: RLambda -> [DotEdge String]
edges (RIndex _ _) = []
edges (RAbs t r) = pointEdge r (label t) : edges t
edges (RApp t t' r) = pointEdge r (label t)
    : pointEdge r (label t')
    : edges t ++ edges t'

randomColor :: IO Color
randomColor = do
    r <- randomIO
    g <- randomIO
    b <- randomIO
    return RGB { red   = r
               , green = g
               , blue  = b
               }

indices :: RLambda -> IO (Map Int Color)
indices (RAbs n _) = indices n
indices (RApp t t' _) = do
    idxT  <- indices t
    idxT' <- indices t'
    return $ M.union idxT idxT'
indices (RIndex n _) = do
    c <- randomColor
    return $ M.singleton n c

degColor :: Map Int Color -> Int -> Attribute
degColor f n = FillColor $ toColorList [color]
    where color = (M.!) f n

-- | Given a tree produces a suitable graphviz representation.
toGraph :: L.Lambda -> IO (DotGraph String)
toGraph t = do
    let t' = fst $ rankL 0 t
    f <- indices t'

    let nodes' = nodes f t'
    let edges' = edges t'
    return DotGraph { strictGraph = False
                    , directedGraph = False
                    , graphID = Nothing
                    , graphStatements = DotStmts {
                        attrStmts = [NodeAttrs [shape Circle
                                               ,style filled
                                               ,fillColor White
                                               ,FixedSize SetNodeSize
                                               ,Width 0.4]]
                        , subGraphs = []
                        , nodeStmts = nodes'
                        , edgeStmts = edges'
                      }
                 }

toDotRepresentation :: L.Lambda -> IO String
toDotRepresentation t = do
    x <- toGraph t
    let d = renderDot $ toDot x
    return $ B.unpack d
