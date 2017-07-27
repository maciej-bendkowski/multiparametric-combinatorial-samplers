-- | Lambda term sampler sitting on top of Sampler.hs
-- | Note: The package is meant for testing purposes only.
-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Main
       (main)
       where

import qualified Sampler as S
import System.Environment

data DeBruijn = S DeBruijn
              | Z
              deriving (Show)

data Lambda = Abs Lambda
            | App Lambda Lambda
            | Index DeBruijn
            deriving (Show)

toDeBruijn :: S.DB -> DeBruijn
toDeBruijn (S.S d) = S $ toDeBruijn d
toDeBruijn S.Z     = Z

toLambdaA :: S.A -> Lambda
toLambdaA (S.AppA a t)     = App (toLambdaA a) (toLambda t)
toLambdaA (S.AppIndex d t) = App (Index $ toDeBruijn d) (toLambda t)
toLambdaA (S.Redex t t')   = App (Abs $ toLambda t) (toLambda t')

toLambda :: S.L -> Lambda
toLambda (S.Index d) = Index $ toDeBruijn d
toLambda (S.Abs t)   = Abs $ toLambda t
toLambda (S.App a)   = toLambdaA a

sizeDB :: DeBruijn -> Int
sizeDB (S d) = 1 + sizeDB d
sizeDB Z     = 1

size :: Lambda -> Int
size (Abs t)    = 1 + size t
size (App t t') = 1 + size t + size t'
size (Index d)  = sizeDB d

redexes :: Lambda -> Int
redexes (App (Abs t) t') = 2 + redexes t + redexes t'
redexes (App t t')       = redexes t + redexes t'
redexes (Abs t)          = redexes t
redexes (Index _)        = 0

main :: IO ()
main = do
    args <- getArgs
    let lb = read (head args) :: Int
    let ub = read (args !! 1) :: Int
    t <- S.sampleLIO lb ub
    let t' = toLambda t
    let n = size t'
    let r = redexes t'
    print $ fromIntegral r / fromIntegral n
