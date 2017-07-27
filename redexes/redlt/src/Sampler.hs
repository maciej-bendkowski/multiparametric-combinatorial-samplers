-- | Compiler: Boltzmann brain v1.1
-- | Singularity: 0.259621540949258
-- | System type: algebraic
module Sampler
       (A(..), DB(..), L(..), genRandomA, genRandomDB, genRandomL,
        sampleA, sampleDB, sampleL, sampleAIO, sampleDBIO, sampleLIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

data A = AppA A L
       | AppIndex DB L
       | Redex L L

data DB = S DB
        | Z

data L = Abs L
       | App A
       | Index DB

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomA :: RandomGen g => Int -> MaybeT (Rand g) (A, Int)
genRandomA ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.24592568833080752 then
         do (x0, w0) <- genRandomA (ub - 1)
            (x1, w1) <- genRandomL (ub - 1 - w0)
            return (AppA x0 x1, 1 + w1 + w0)
         else
         if p < 0.49185137572639215 then
           do (x0, w0) <- genRandomDB (ub - 1)
              (x1, w1) <- genRandomL (ub - 1 - w0)
              return (AppIndex x0 x1, 1 + w1 + w0)
           else
           do (x0, w0) <- genRandomL (ub - 2)
              (x1, w1) <- genRandomL (ub - 2 - w0)
              return (Redex x0 x1, 2 + w1 + w0)

genRandomDB :: RandomGen g => Int -> MaybeT (Rand g) (DB, Int)
genRandomDB ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.259621540949258 then
         do (x0, w0) <- genRandomDB (ub - 1)
            return (S x0, 1 + w0)
         else return (Z, 1)

genRandomL :: RandomGen g => Int -> MaybeT (Rand g) (L, Int)
genRandomL ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.259621540949258 then
         do (x0, w0) <- genRandomL (ub - 1)
            return (Abs x0, 1 + w0)
         else
         if p < 0.6298107706482814 then
           do (x0, w0) <- genRandomA ub
              return (App x0, w0)
           else
           do (x0, w0) <- genRandomDB ub
              return (Index x0, w0)

sampleA :: RandomGen g => Int -> Int -> Rand g A
sampleA lb ub
  = do sample <- runMaybeT (genRandomA ub)
       case sample of
           Nothing -> sampleA lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleA lb ub

sampleDB :: RandomGen g => Int -> Int -> Rand g DB
sampleDB lb ub
  = do sample <- runMaybeT (genRandomDB ub)
       case sample of
           Nothing -> sampleDB lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleDB lb ub

sampleL :: RandomGen g => Int -> Int -> Rand g L
sampleL lb ub
  = do sample <- runMaybeT (genRandomL ub)
       case sample of
           Nothing -> sampleL lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleL lb ub

sampleAIO :: Int -> Int -> IO A
sampleAIO lb ub = evalRandIO (sampleA lb ub)

sampleDBIO :: Int -> Int -> IO DB
sampleDBIO lb ub = evalRandIO (sampleDB lb ub)

sampleLIO :: Int -> Int -> IO L
sampleLIO lb ub = evalRandIO (sampleL lb ub)
