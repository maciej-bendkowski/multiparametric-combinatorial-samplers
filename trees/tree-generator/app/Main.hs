-- | Tree sampler visualiser sitting on top of Sampler.hs
-- | Note: The package is meant for testing purposes only.
-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Main
       (main)
       where

import Sampler (sampleTreeIO)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let lb = read (head args) :: Int
    let ub = read (args !! 1) :: Int
    t <- sampleTreeIO lb ub
    print t
