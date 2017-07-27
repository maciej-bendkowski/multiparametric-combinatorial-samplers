{-|
 - Module      : Visualisation
 - Description : Tree visualization utilities.
 - Copyright   : (c) Maciej Bendkowski, 2017
 -
 - License     : BSD3
 - Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 - Stability   : experimental
 -}
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

import Trees

data RTree = RNode [RTree] Int

randomColor :: IO Color
randomColor = do
    r <- randomIO
    g <- randomIO
    b <- randomIO
    return RGB { red   = r
               , green = g
               , blue  = b
               }

degrees :: RTree -> IO (Map Int Color)
degrees (RNode ts r) = do
    ms <- mapM degrees ts
    let f = M.unions ms
    let n = length ts
    case n `M.lookup` f of
      Just _ -> return f
      Nothing -> do
          c <- randomColor
          return $ M.insert n c f

degColor :: Map Int Color -> Int -> Attribute
degColor f n = FillColor $ toColorList [color]
    where color = (M.!) f n

-- | Gets the label of the given ranked tree.
label :: RTree -> Int
label (RNode _ r) = r

-- | Ranks the tree with unique integer
--   labels starting with the given number r.
rankL :: Int -> Tree -> (RTree, Int)
rankL r (Node ts) = (RNode ts' r, r')
    where (ts', r') = rankL' (r+1) ts

rankL' r [] = ([], r)
rankL' r (x:xs) = (x':xs', r'')
    where (x', r') = rankL r x
          (xs', r'') = rankL' r' xs

-- | Single point node.
pointNode :: Map Int Color -> Int -> Int -> DotNode String
pointNode f n deg = DotNode (show n) [degColor f deg]

-- | Constructs a list of graph nodes based on the given ranked tree.
nodes :: Map Int Color -> RTree -> [DotNode String]
nodes f (RNode ts r) = pointNode f r (length ts) : concatMap (nodes f) ts

-- | Single point edge.
pointEdge :: Int -> Int -> DotEdge String
pointEdge n k = DotEdge (show n) (show k) []

-- | Constructs a list of graph edges
--   based on the given ranked tree.
edges :: RTree -> [DotEdge String]
edges (RNode ts r) = es ++ concatMap edges ts
    where es = map (pointEdge r) ls
          ls = map label ts

-- | Given a tree produces a suitable graphviz representation.
toGraph :: Tree -> IO (DotGraph String)
toGraph t = do
    let t' = fst $ rankL 0 t
    f <- degrees t'

    let nodes' = nodes f t'
    let edges' = edges t'
    return DotGraph { strictGraph = False
                    , directedGraph = False
                    , graphID = Nothing
                    , graphStatements = DotStmts {
                        attrStmts = [NodeAttrs [style filled
                                               ,color Black
                                               ,shape Circle
                                               ,toLabel ""
                                               ,Width 0.4]]
                        , subGraphs = []
                        , nodeStmts = nodes'
                        , edgeStmts = edges'
                      }
                 }

toDotRepresentation :: Tree -> IO String
toDotRepresentation t = do
    x <- toGraph t
    let d = renderDot $toDot x
    return $ B.unpack d
