-- | Compiler: Boltzmann brain v1.1
-- | Singularity: 0.295597738799039
-- | System type: algebraic
module Sampler
       (DeBruijn(..), DeBruijnR(..), Lambda(..), genRandomDeBruijn,
        genRandomDeBruijnR, genRandomLambda, sampleDeBruijn,
        sampleDeBruijnR, sampleLambda, sampleDeBruijnIO, sampleDeBruijnRIO,
        sampleLambdaIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

data DeBruijn = Var0
              | Var1
              | Var2
              | Var3
              | Var4
              | Var5
              | Var6
              | Var7
              | Var8
              | Var9
              | Var10
              | Var11
              | Var12
              | Var13
              | Var14
              | Var15
              | Var16
              | Var17
              | Var18
              | Var19
              | Var20
              | IndexR DeBruijnR

data DeBruijnR = S DeBruijnR
               | Z

data Lambda = Abs Lambda
            | App Lambda Lambda
            | Index DeBruijn

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomDeBruijn ::
                    RandomGen g => Int -> MaybeT (Rand g) (DeBruijn, Int)
genRandomDeBruijn ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.7044022354730344 then return (Var0, 1) else
         if p < 0.9126219434838515 then return (Var1, 2) else
           if p < 0.9741712183452452 then return (Var2, 3) else
             if p < 0.9923650448189937 then return (Var3, 4) else
               if p < 0.9977430987847358 then return (Var4, 5) else
                 if p < 0.9993328393761484 then return (Var5, 6) else
                   if p < 0.999802763100247 then return (Var6, 7) else
                     if p < 0.9999416714904986 then return (Var7, 8) else
                       if p < 0.9999827324965571 then return (Var8, 9) else
                         if p < 0.9999948700371009 then return (Var9, 10) else
                           if p < 0.9999984578666402 then return (Var10, 11) else
                             if p < 0.9999995184209391 then return (Var11, 12) else
                               if p < 0.9999998319183918 then return (Var12, 13) else
                                 if p < 0.9999999245875298 then return (Var13, 14) else
                                   if p < 0.9999999519803177 then return (Var14, 15) else
                                     if p < 0.9999999600775636 then return (Var15, 16) else
                                       if p < 0.9999999624710914 then return (Var16, 17) else
                                         if p < 0.9999999631786127 then return (Var17, 18) else
                                           if p < 0.9999999633877544 then return (Var18, 19) else
                                             if p < 0.9999999634495763 then return (Var19, 20) else
                                               if p < 0.9999999634678507 then return (Var20, 21)
                                                 else
                                                 do (x0, w0) <- genRandomDeBruijnR ub
                                                    return (IndexR x0, w0)

genRandomDeBruijnR ::
                     RandomGen g => Int -> MaybeT (Rand g) (DeBruijnR, Int)
genRandomDeBruijnR ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.295597738799039 then
         do (x0, w0) <- genRandomDeBruijnR (ub - 1)
            return (S x0, 1 + w0)
         else return (Z, 22)

genRandomLambda ::
                  RandomGen g => Int -> MaybeT (Rand g) (Lambda, Int)
genRandomLambda ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.295597738799039 then
         do (x0, w0) <- genRandomLambda (ub - 1)
            return (Abs x0, 1 + w0)
         else
         if p < 0.6477988683550946 then
           do (x0, w0) <- genRandomLambda (ub - 1)
              (x1, w1) <- genRandomLambda (ub - 1 - w0)
              return (App x0 x1, 1 + w1 + w0)
           else
           do (x0, w0) <- genRandomDeBruijn ub
              return (Index x0, w0)

sampleDeBruijn :: RandomGen g => Int -> Int -> Rand g DeBruijn
sampleDeBruijn lb ub
  = do sample <- runMaybeT (genRandomDeBruijn ub)
       case sample of
           Nothing -> sampleDeBruijn lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleDeBruijn lb ub

sampleDeBruijnR :: RandomGen g => Int -> Int -> Rand g DeBruijnR
sampleDeBruijnR lb ub
  = do sample <- runMaybeT (genRandomDeBruijnR ub)
       case sample of
           Nothing -> sampleDeBruijnR lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleDeBruijnR lb ub

sampleLambda :: RandomGen g => Int -> Int -> Rand g Lambda
sampleLambda lb ub
  = do sample <- runMaybeT (genRandomLambda ub)
       case sample of
           Nothing -> sampleLambda lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleLambda lb ub

sampleDeBruijnIO :: Int -> Int -> IO DeBruijn
sampleDeBruijnIO lb ub = evalRandIO (sampleDeBruijn lb ub)

sampleDeBruijnRIO :: Int -> Int -> IO DeBruijnR
sampleDeBruijnRIO lb ub = evalRandIO (sampleDeBruijnR lb ub)

sampleLambdaIO :: Int -> Int -> IO Lambda
sampleLambdaIO lb ub = evalRandIO (sampleLambda lb ub)
