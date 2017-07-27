-- | Lambda term sampler sitting on top of Sampler.hs
-- | Note: The package is meant for testing purposes only.
-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Main
       (main)
       where

import System.IO
import System.Environment

import qualified Lambda as L
import qualified Sampler as S
import qualified Visualisation as V

printDistribution :: L.Lambda -> IO ()
printDistribution t = do
    let n = L.size t
    hPutStrLn stderr "Index distribution:"
    mapM_ (hPutStrLn stderr) (L.distribution t n)

main :: IO ()
main = do
    args <- getArgs
    let lb = read (head args) :: Int
    let ub = read (args !! 1) :: Int
    t <- S.sampleLambdaIO lb ub
    let t' = L.toLambda t
    printDistribution t'
    d <- V.toDotRepresentation t'
    putStrLn d
