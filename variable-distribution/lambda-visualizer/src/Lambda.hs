-- | Lambda term sampler utilities.
-- | Note: The package is meant for testing purposes only.
-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Lambda
       ( Lambda(..)
       , DeBruijn(..)
       , toLambda
       , size
       , distribution
       , value)
       where

import qualified Sampler as S

import Data.MultiSet (MultiSet, Occur)
import qualified Data.MultiSet as M

data Lambda = Abs Lambda
            | App Lambda Lambda
            | Index DeBruijn
            deriving (Show)

data DeBruijn = S DeBruijn
              | Z
              deriving (Show)

nth :: Int -> DeBruijn
nth 0 = Z
nth n = S $ nth (n-1)

toDeBruijnR :: S.DeBruijnR -> DeBruijn
toDeBruijnR (S.S n) = S $ toDeBruijnR n
toDeBruijnR S.Z = nth 21

toDeBruijn :: S.DeBruijn -> DeBruijn
toDeBruijn S.Var0       = nth 0
toDeBruijn S.Var1       = nth 1
toDeBruijn S.Var2       = nth 2
toDeBruijn S.Var3       = nth 3
toDeBruijn S.Var4       = nth 4
toDeBruijn S.Var5       = nth 5
toDeBruijn S.Var6       = nth 6
toDeBruijn S.Var7       = nth 7
toDeBruijn S.Var8       = nth 8
toDeBruijn S.Var9       = nth 9
toDeBruijn S.Var10      = nth 10
toDeBruijn S.Var11      = nth 11
toDeBruijn S.Var12      = nth 12
toDeBruijn S.Var13      = nth 13
toDeBruijn S.Var14      = nth 14
toDeBruijn S.Var15      = nth 15
toDeBruijn S.Var16      = nth 16
toDeBruijn S.Var17      = nth 17
toDeBruijn S.Var18      = nth 18
toDeBruijn S.Var19      = nth 19
toDeBruijn S.Var20      = nth 20
toDeBruijn (S.IndexR n) = toDeBruijnR n

toLambda :: S.Lambda -> Lambda
toLambda (S.Abs t)    = Abs $ toLambda t
toLambda (S.App t t') = App (toLambda t) (toLambda t')
toLambda (S.Index n)  = Index $ toDeBruijn n

size :: Lambda -> Int
size (Abs t)    = 1 + size t
size (App t t') = 1 + size t + size t'
size (Index n)  = sizeD n

sizeD :: DeBruijn -> Int
sizeD (S n) = 1 + sizeD n
sizeD Z     = 1

value :: DeBruijn -> Int
value d = sizeD d - 1

indices :: Lambda -> MultiSet Int
indices (Abs t)    = indices t
indices (App t t') = indices t `M.union` indices t'
indices (Index n)  = M.singleton $ value n

distribution :: Lambda -> Int -> [String]
distribution t n = map fun s
    where s = M.toOccurList $ indices t
          fun (idx,f) = let x = fromIntegral (f * y) / fromIntegral n
                            y = idx + 1 in
                              show idx ++ ": " ++ show x
