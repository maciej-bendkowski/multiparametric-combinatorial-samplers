-- | Lambda term sampler sitting on top of Sampler.hs
-- | Note: The package is meant for testing purposes only.
-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Main
       (main)
       where

import System.Environment
import Sampler

sizeDeBruijn :: DeBruijn -> Int
sizeDeBruijn (S d) = 1 + sizeDeBruijn d
sizeDeBruijn Z     = 2

size :: Lambda -> Int
size (Abs t)    = 2 + size t
size (App t t') = 2 + size t + size t'
size (Index d)  = sizeDeBruijn d

abstractions :: Lambda -> Int
abstractions (Abs t)    = 2 + abstractions t
abstractions (App t t') = abstractions t + abstractions t'
abstractions (Index _)  = 0

main :: IO ()
main = do
    args <- getArgs
    let lb = read (head args) :: Int
    let ub = read (args !! 1) :: Int
    t <- sampleLambdaIO lb ub
    let n = size t
    let r = abstractions t
    print $ fromIntegral r / fromIntegral n
