-- | Compiler: Boltzmann brain v1.1
-- | Singularity: 0.180690512279176
-- | System type: algebraic
module Sampler (Tree(..), genRandomTree, sampleTree, sampleTreeIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

data Tree = Node0
          | Node1 Tree
          | Node2 Tree Tree
          | Node3 Tree Tree Tree
          | Node4 Tree Tree Tree Tree
          | Node5 Tree Tree Tree Tree Tree
          | Node6 Tree Tree Tree Tree Tree Tree
          | Node7 Tree Tree Tree Tree Tree Tree Tree
          | Node8 Tree Tree Tree Tree Tree Tree Tree Tree
          | Node9 Tree Tree Tree Tree Tree Tree Tree Tree Tree
          | Node10 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
          | Node11 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
          | Node12 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree
          | Node13 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree
          | Node14 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree
          | Node15 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree
          | Node16 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree
          | Node17 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree
          | Node18 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree Tree
          | Node19 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree Tree Tree
          | Node20 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree Tree Tree Tree
          | Node21 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
          | Node22 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
          | Node23 Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree
                   Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree Tree

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomTree :: RandomGen g => Int -> MaybeT (Rand g) (Tree, Int)
genRandomTree ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.6837975470756853 then return (Node0, 1) else
         if p < 0.8644880593548613 then
           do (x0, w0) <- genRandomTree (ub - 1)
              return (Node1 x0, 1 + w0)
           else
           if p < 0.9122347372529283 then
             do (x0, w0) <- genRandomTree (ub - 1)
                (x1, w1) <- genRandomTree (ub - 1 - w0)
                return (Node2 x0 x1, 1 + w1 + w0)
             else
             if p < 0.9223394501237985 then
               do (x0, w0) <- genRandomTree (ub - 1)
                  (x1, w1) <- genRandomTree (ub - 1 - w0)
                  (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                  return (Node3 x0 x1 x2, 1 + w2 + w1 + w0)
               else
               if p < 0.9317978733836517 then
                 do (x0, w0) <- genRandomTree (ub - 1)
                    (x1, w1) <- genRandomTree (ub - 1 - w0)
                    (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                    (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                    return (Node4 x0 x1 x2 x3, 1 + w3 + w2 + w1 + w0)
                 else
                 if p < 0.940612267340251 then
                   do (x0, w0) <- genRandomTree (ub - 1)
                      (x1, w1) <- genRandomTree (ub - 1 - w0)
                      (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                      (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                      (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                      return (Node5 x0 x1 x2 x3 x4, 1 + w4 + w3 + w2 + w1 + w0)
                   else
                   if p < 0.94878520628892 then
                     do (x0, w0) <- genRandomTree (ub - 1)
                        (x1, w1) <- genRandomTree (ub - 1 - w0)
                        (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                        (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                        (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                        (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                        return (Node6 x0 x1 x2 x3 x4 x5, 1 + w5 + w4 + w3 + w2 + w1 + w0)
                     else
                     if p < 0.9563196470536166 then
                       do (x0, w0) <- genRandomTree (ub - 1)
                          (x1, w1) <- genRandomTree (ub - 1 - w0)
                          (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                          (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                          (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                          (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                          (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                          return
                            (Node7 x0 x1 x2 x3 x4 x5 x6, 1 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
                       else
                       if p < 0.9632190185462579 then
                         do (x0, w0) <- genRandomTree (ub - 1)
                            (x1, w1) <- genRandomTree (ub - 1 - w0)
                            (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                            (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                            (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                            (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                            (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                            (x7, w7) <- genRandomTree
                                          (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                            return
                              (Node8 x0 x1 x2 x3 x4 x5 x6 x7,
                               1 + w7 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
                         else
                         if p < 0.9694873408671456 then
                           do (x0, w0) <- genRandomTree (ub - 1)
                              (x1, w1) <- genRandomTree (ub - 1 - w0)
                              (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                              (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                              (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                              (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                              (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                              (x7, w7) <- genRandomTree
                                            (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                              (x8, w8) <- genRandomTree
                                            (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                              return
                                (Node9 x0 x1 x2 x3 x4 x5 x6 x7 x8,
                                 1 + w8 + w7 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
                           else
                           if p < 0.9751293868688483 then
                             do (x0, w0) <- genRandomTree (ub - 1)
                                (x1, w1) <- genRandomTree (ub - 1 - w0)
                                (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                (x7, w7) <- genRandomTree
                                              (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                (x8, w8) <- genRandomTree
                                              (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                                (x9, w9) <- genRandomTree
                                              (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 - w8)
                                return
                                  (Node10 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9,
                                   1 + w9 + w8 + w7 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
                             else
                             if p < 0.980150906319679 then
                               do (x0, w0) <- genRandomTree (ub - 1)
                                  (x1, w1) <- genRandomTree (ub - 1 - w0)
                                  (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                  (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                  (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                  (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                  (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                  (x7, w7) <- genRandomTree
                                                (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                  (x8, w8) <- genRandomTree
                                                (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                                  (x9, w9) <- genRandomTree
                                                (ub -
                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 - w8)
                                  (x10, w10) <- genRandomTree
                                                  (ub -
                                                     1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 - w8
                                                       - w9)
                                  return
                                    (Node11 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10,
                                     1 + w10 + w9 + w8 + w7 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
                               else
                               if p < 0.9845589450117991 then
                                 do (x0, w0) <- genRandomTree (ub - 1)
                                    (x1, w1) <- genRandomTree (ub - 1 - w0)
                                    (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                    (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                    (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                    (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                    (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                    (x7, w7) <- genRandomTree
                                                  (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                    (x8, w8) <- genRandomTree
                                                  (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                                    (x9, w9) <- genRandomTree
                                                  (ub -
                                                     1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 - w8)
                                    (x10, w10) <- genRandomTree
                                                    (ub -
                                                       1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                         w8
                                                         - w9)
                                    (x11, w11) <- genRandomTree
                                                    (ub -
                                                       1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                         w8
                                                         - w9
                                                         - w10)
                                    return
                                      (Node12 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11,
                                       1 +
                                         w11 + w10 + w9 + w8 + w7 + w6 + w5 + w4 + w3 + w2 + w1 +
                                           w0)
                                 else
                                 if p < 0.9883623125439835 then
                                   do (x0, w0) <- genRandomTree (ub - 1)
                                      (x1, w1) <- genRandomTree (ub - 1 - w0)
                                      (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                      (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                      (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                      (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                      (x6, w6) <- genRandomTree
                                                    (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                      (x7, w7) <- genRandomTree
                                                    (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                      (x8, w8) <- genRandomTree
                                                    (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                                      (x9, w9) <- genRandomTree
                                                    (ub -
                                                       1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                         w8)
                                      (x10, w10) <- genRandomTree
                                                      (ub -
                                                         1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                           w8
                                                           - w9)
                                      (x11, w11) <- genRandomTree
                                                      (ub -
                                                         1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                           w8
                                                           - w9
                                                           - w10)
                                      (x12, w12) <- genRandomTree
                                                      (ub -
                                                         1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                           w8
                                                           - w9
                                                           - w10
                                                           - w11)
                                      return
                                        (Node13 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12,
                                         1 +
                                           w12 + w11 + w10 + w9 + w8 + w7 + w6 + w5 + w4 + w3 + w2 +
                                             w1
                                             + w0)
                                   else
                                   if p < 0.9915722913415054 then
                                     do (x0, w0) <- genRandomTree (ub - 1)
                                        (x1, w1) <- genRandomTree (ub - 1 - w0)
                                        (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                        (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                        (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                        (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                        (x6, w6) <- genRandomTree
                                                      (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                        (x7, w7) <- genRandomTree
                                                      (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                        (x8, w8) <- genRandomTree
                                                      (ub -
                                                         1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                                        (x9, w9) <- genRandomTree
                                                      (ub -
                                                         1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7 -
                                                           w8)
                                        (x10, w10) <- genRandomTree
                                                        (ub -
                                                           1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7
                                                             - w8
                                                             - w9)
                                        (x11, w11) <- genRandomTree
                                                        (ub -
                                                           1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7
                                                             - w8
                                                             - w9
                                                             - w10)
                                        (x12, w12) <- genRandomTree
                                                        (ub -
                                                           1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7
                                                             - w8
                                                             - w9
                                                             - w10
                                                             - w11)
                                        (x13, w13) <- genRandomTree
                                                        (ub -
                                                           1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7
                                                             - w8
                                                             - w9
                                                             - w10
                                                             - w11
                                                             - w12)
                                        return
                                          (Node14 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13,
                                           1 +
                                             w13 + w12 + w11 + w10 + w9 + w8 + w7 + w6 + w5 + w4 +
                                               w3
                                               + w2
                                               + w1
                                               + w0)
                                     else
                                     if p < 0.9942037525000373 then
                                       do (x0, w0) <- genRandomTree (ub - 1)
                                          (x1, w1) <- genRandomTree (ub - 1 - w0)
                                          (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                          (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                          (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                          (x5, w5) <- genRandomTree
                                                        (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                          (x6, w6) <- genRandomTree
                                                        (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                          (x7, w7) <- genRandomTree
                                                        (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                          (x8, w8) <- genRandomTree
                                                        (ub -
                                                           1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                             w7)
                                          (x9, w9) <- genRandomTree
                                                        (ub -
                                                           1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7
                                                             - w8)
                                          (x10, w10) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7
                                                               - w8
                                                               - w9)
                                          (x11, w11) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7
                                                               - w8
                                                               - w9
                                                               - w10)
                                          (x12, w12) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7
                                                               - w8
                                                               - w9
                                                               - w10
                                                               - w11)
                                          (x13, w13) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7
                                                               - w8
                                                               - w9
                                                               - w10
                                                               - w11
                                                               - w12)
                                          (x14, w14) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7
                                                               - w8
                                                               - w9
                                                               - w10
                                                               - w11
                                                               - w12
                                                               - w13)
                                          return
                                            (Node15 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
                                               x14,
                                             1 +
                                               w14 + w13 + w12 + w11 + w10 + w9 + w8 + w7 + w6 + w5
                                                 + w4
                                                 + w3
                                                 + w2
                                                 + w1
                                                 + w0)
                                       else
                                       if p < 0.9962769849825724 then
                                         do (x0, w0) <- genRandomTree (ub - 1)
                                            (x1, w1) <- genRandomTree (ub - 1 - w0)
                                            (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                            (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                            (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                            (x5, w5) <- genRandomTree
                                                          (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                            (x6, w6) <- genRandomTree
                                                          (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                            (x7, w7) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                            (x8, w8) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7)
                                            (x9, w9) <- genRandomTree
                                                          (ub -
                                                             1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 -
                                                               w7
                                                               - w8)
                                            (x10, w10) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8
                                                                 - w9)
                                            (x11, w11) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8
                                                                 - w9
                                                                 - w10)
                                            (x12, w12) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8
                                                                 - w9
                                                                 - w10
                                                                 - w11)
                                            (x13, w13) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8
                                                                 - w9
                                                                 - w10
                                                                 - w11
                                                                 - w12)
                                            (x14, w14) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8
                                                                 - w9
                                                                 - w10
                                                                 - w11
                                                                 - w12
                                                                 - w13)
                                            (x15, w15) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8
                                                                 - w9
                                                                 - w10
                                                                 - w11
                                                                 - w12
                                                                 - w13
                                                                 - w14)
                                            return
                                              (Node16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
                                                 x14
                                                 x15,
                                               1 +
                                                 w15 + w14 + w13 + w12 + w11 + w10 + w9 + w8 + w7 +
                                                   w6
                                                   + w5
                                                   + w4
                                                   + w3
                                                   + w2
                                                   + w1
                                                   + w0)
                                         else
                                         if p < 0.9978208149345955 then
                                           do (x0, w0) <- genRandomTree (ub - 1)
                                              (x1, w1) <- genRandomTree (ub - 1 - w0)
                                              (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                              (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                              (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                                              (x5, w5) <- genRandomTree
                                                            (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                              (x6, w6) <- genRandomTree
                                                            (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                              (x7, w7) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                                              (x8, w8) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7)
                                              (x9, w9) <- genRandomTree
                                                            (ub -
                                                               1 - w0 - w1 - w2 - w3 - w4 - w5 - w6
                                                                 - w7
                                                                 - w8)
                                              (x10, w10) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9)
                                              (x11, w11) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9
                                                                   - w10)
                                              (x12, w12) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9
                                                                   - w10
                                                                   - w11)
                                              (x13, w13) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9
                                                                   - w10
                                                                   - w11
                                                                   - w12)
                                              (x14, w14) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9
                                                                   - w10
                                                                   - w11
                                                                   - w12
                                                                   - w13)
                                              (x15, w15) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9
                                                                   - w10
                                                                   - w11
                                                                   - w12
                                                                   - w13
                                                                   - w14)
                                              (x16, w16) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8
                                                                   - w9
                                                                   - w10
                                                                   - w11
                                                                   - w12
                                                                   - w13
                                                                   - w14
                                                                   - w15)
                                              return
                                                (Node17 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
                                                   x13
                                                   x14
                                                   x15
                                                   x16,
                                                 1 +
                                                   w16 + w15 + w14 + w13 + w12 + w11 + w10 + w9 + w8
                                                     + w7
                                                     + w6
                                                     + w5
                                                     + w4
                                                     + w3
                                                     + w2
                                                     + w1
                                                     + w0)
                                           else
                                           if p < 0.9988780474506945 then
                                             do (x0, w0) <- genRandomTree (ub - 1)
                                                (x1, w1) <- genRandomTree (ub - 1 - w0)
                                                (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                                (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                                (x4, w4) <- genRandomTree
                                                              (ub - 1 - w0 - w1 - w2 - w3)
                                                (x5, w5) <- genRandomTree
                                                              (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                                (x6, w6) <- genRandomTree
                                                              (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                                                (x7, w7) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6)
                                                (x8, w8) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7)
                                                (x9, w9) <- genRandomTree
                                                              (ub -
                                                                 1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                   w6
                                                                   - w7
                                                                   - w8)
                                                (x10, w10) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9)
                                                (x11, w11) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10)
                                                (x12, w12) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10
                                                                     - w11)
                                                (x13, w13) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10
                                                                     - w11
                                                                     - w12)
                                                (x14, w14) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10
                                                                     - w11
                                                                     - w12
                                                                     - w13)
                                                (x15, w15) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10
                                                                     - w11
                                                                     - w12
                                                                     - w13
                                                                     - w14)
                                                (x16, w16) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10
                                                                     - w11
                                                                     - w12
                                                                     - w13
                                                                     - w14
                                                                     - w15)
                                                (x17, w17) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8
                                                                     - w9
                                                                     - w10
                                                                     - w11
                                                                     - w12
                                                                     - w13
                                                                     - w14
                                                                     - w15
                                                                     - w16)
                                                return
                                                  (Node18 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17,
                                                   1 +
                                                     w17 + w16 + w15 + w14 + w13 + w12 + w11 + w10 +
                                                       w9
                                                       + w8
                                                       + w7
                                                       + w6
                                                       + w5
                                                       + w4
                                                       + w3
                                                       + w2
                                                       + w1
                                                       + w0)
                                             else
                                             if p < 0.9995145239593862 then
                                               do (x0, w0) <- genRandomTree (ub - 1)
                                                  (x1, w1) <- genRandomTree (ub - 1 - w0)
                                                  (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                                  (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                                                  (x4, w4) <- genRandomTree
                                                                (ub - 1 - w0 - w1 - w2 - w3)
                                                  (x5, w5) <- genRandomTree
                                                                (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                                  (x6, w6) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5)
                                                  (x7, w7) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6)
                                                  (x8, w8) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7)
                                                  (x9, w9) <- genRandomTree
                                                                (ub -
                                                                   1 - w0 - w1 - w2 - w3 - w4 - w5 -
                                                                     w6
                                                                     - w7
                                                                     - w8)
                                                  (x10, w10) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9)
                                                  (x11, w11) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10)
                                                  (x12, w12) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11)
                                                  (x13, w13) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11
                                                                       - w12)
                                                  (x14, w14) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11
                                                                       - w12
                                                                       - w13)
                                                  (x15, w15) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11
                                                                       - w12
                                                                       - w13
                                                                       - w14)
                                                  (x16, w16) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11
                                                                       - w12
                                                                       - w13
                                                                       - w14
                                                                       - w15)
                                                  (x17, w17) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11
                                                                       - w12
                                                                       - w13
                                                                       - w14
                                                                       - w15
                                                                       - w16)
                                                  (x18, w18) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8
                                                                       - w9
                                                                       - w10
                                                                       - w11
                                                                       - w12
                                                                       - w13
                                                                       - w14
                                                                       - w15
                                                                       - w16
                                                                       - w17)
                                                  return
                                                    (Node19 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18,
                                                     1 +
                                                       w18 + w17 + w16 + w15 + w14 + w13 + w12 + w11
                                                         + w10
                                                         + w9
                                                         + w8
                                                         + w7
                                                         + w6
                                                         + w5
                                                         + w4
                                                         + w3
                                                         + w2
                                                         + w1
                                                         + w0)
                                               else
                                               if p < 0.9998298404932269 then
                                                 do (x0, w0) <- genRandomTree (ub - 1)
                                                    (x1, w1) <- genRandomTree (ub - 1 - w0)
                                                    (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                                    (x3, w3) <- genRandomTree
                                                                  (ub - 1 - w0 - w1 - w2)
                                                    (x4, w4) <- genRandomTree
                                                                  (ub - 1 - w0 - w1 - w2 - w3)
                                                    (x5, w5) <- genRandomTree
                                                                  (ub - 1 - w0 - w1 - w2 - w3 - w4)
                                                    (x6, w6) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 -
                                                                       w5)
                                                    (x7, w7) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6)
                                                    (x8, w8) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7)
                                                    (x9, w9) <- genRandomTree
                                                                  (ub -
                                                                     1 - w0 - w1 - w2 - w3 - w4 - w5
                                                                       - w6
                                                                       - w7
                                                                       - w8)
                                                    (x10, w10) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9)
                                                    (x11, w11) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10)
                                                    (x12, w12) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11)
                                                    (x13, w13) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12)
                                                    (x14, w14) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12
                                                                         - w13)
                                                    (x15, w15) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12
                                                                         - w13
                                                                         - w14)
                                                    (x16, w16) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12
                                                                         - w13
                                                                         - w14
                                                                         - w15)
                                                    (x17, w17) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12
                                                                         - w13
                                                                         - w14
                                                                         - w15
                                                                         - w16)
                                                    (x18, w18) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12
                                                                         - w13
                                                                         - w14
                                                                         - w15
                                                                         - w16
                                                                         - w17)
                                                    (x19, w19) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8
                                                                         - w9
                                                                         - w10
                                                                         - w11
                                                                         - w12
                                                                         - w13
                                                                         - w14
                                                                         - w15
                                                                         - w16
                                                                         - w17
                                                                         - w18)
                                                    return
                                                      (Node20 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
                                                         x12
                                                         x13
                                                         x14
                                                         x15
                                                         x16
                                                         x17
                                                         x18
                                                         x19,
                                                       1 +
                                                         w19 + w18 + w17 + w16 + w15 + w14 + w13 +
                                                           w12
                                                           + w11
                                                           + w10
                                                           + w9
                                                           + w8
                                                           + w7
                                                           + w6
                                                           + w5
                                                           + w4
                                                           + w3
                                                           + w2
                                                           + w1
                                                           + w0)
                                                 else
                                                 if p < 0.9999515450855959 then
                                                   do (x0, w0) <- genRandomTree (ub - 1)
                                                      (x1, w1) <- genRandomTree (ub - 1 - w0)
                                                      (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                                      (x3, w3) <- genRandomTree
                                                                    (ub - 1 - w0 - w1 - w2)
                                                      (x4, w4) <- genRandomTree
                                                                    (ub - 1 - w0 - w1 - w2 - w3)
                                                      (x5, w5) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4)
                                                      (x6, w6) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5)
                                                      (x7, w7) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6)
                                                      (x8, w8) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7)
                                                      (x9, w9) <- genRandomTree
                                                                    (ub -
                                                                       1 - w0 - w1 - w2 - w3 - w4 -
                                                                         w5
                                                                         - w6
                                                                         - w7
                                                                         - w8)
                                                      (x10, w10) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9)
                                                      (x11, w11) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10)
                                                      (x12, w12) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11)
                                                      (x13, w13) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12)
                                                      (x14, w14) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13)
                                                      (x15, w15) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13
                                                                           - w14)
                                                      (x16, w16) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13
                                                                           - w14
                                                                           - w15)
                                                      (x17, w17) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13
                                                                           - w14
                                                                           - w15
                                                                           - w16)
                                                      (x18, w18) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13
                                                                           - w14
                                                                           - w15
                                                                           - w16
                                                                           - w17)
                                                      (x19, w19) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13
                                                                           - w14
                                                                           - w15
                                                                           - w16
                                                                           - w17
                                                                           - w18)
                                                      (x20, w20) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8
                                                                           - w9
                                                                           - w10
                                                                           - w11
                                                                           - w12
                                                                           - w13
                                                                           - w14
                                                                           - w15
                                                                           - w16
                                                                           - w17
                                                                           - w18
                                                                           - w19)
                                                      return
                                                        (Node21 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20,
                                                         1 +
                                                           w20 + w19 + w18 + w17 + w16 + w15 + w14 +
                                                             w13
                                                             + w12
                                                             + w11
                                                             + w10
                                                             + w9
                                                             + w8
                                                             + w7
                                                             + w6
                                                             + w5
                                                             + w4
                                                             + w3
                                                             + w2
                                                             + w1
                                                             + w0)
                                                   else
                                                   if p < 0.9999894371077472 then
                                                     do (x0, w0) <- genRandomTree (ub - 1)
                                                        (x1, w1) <- genRandomTree (ub - 1 - w0)
                                                        (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                                        (x3, w3) <- genRandomTree
                                                                      (ub - 1 - w0 - w1 - w2)
                                                        (x4, w4) <- genRandomTree
                                                                      (ub - 1 - w0 - w1 - w2 - w3)
                                                        (x5, w5) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4)
                                                        (x6, w6) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5)
                                                        (x7, w7) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6)
                                                        (x8, w8) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7)
                                                        (x9, w9) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8)
                                                        (x10, w10) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9)
                                                        (x11, w11) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10)
                                                        (x12, w12) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11)
                                                        (x13, w13) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12)
                                                        (x14, w14) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13)
                                                        (x15, w15) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14)
                                                        (x16, w16) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15)
                                                        (x17, w17) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16)
                                                        (x18, w18) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17)
                                                        (x19, w19) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18)
                                                        (x20, w20) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18
                                                                             - w19)
                                                        (x21, w21) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18
                                                                             - w19
                                                                             - w20)
                                                        return
                                                          (Node22 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21,
                                                           1 +
                                                             w21 + w20 + w19 + w18 + w17 + w16 + w15
                                                               + w14
                                                               + w13
                                                               + w12
                                                               + w11
                                                               + w10
                                                               + w9
                                                               + w8
                                                               + w7
                                                               + w6
                                                               + w5
                                                               + w4
                                                               + w3
                                                               + w2
                                                               + w1
                                                               + w0)
                                                     else
                                                     do (x0, w0) <- genRandomTree (ub - 1)
                                                        (x1, w1) <- genRandomTree (ub - 1 - w0)
                                                        (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                                                        (x3, w3) <- genRandomTree
                                                                      (ub - 1 - w0 - w1 - w2)
                                                        (x4, w4) <- genRandomTree
                                                                      (ub - 1 - w0 - w1 - w2 - w3)
                                                        (x5, w5) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4)
                                                        (x6, w6) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5)
                                                        (x7, w7) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6)
                                                        (x8, w8) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7)
                                                        (x9, w9) <- genRandomTree
                                                                      (ub -
                                                                         1 - w0 - w1 - w2 - w3 - w4
                                                                           - w5
                                                                           - w6
                                                                           - w7
                                                                           - w8)
                                                        (x10, w10) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9)
                                                        (x11, w11) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10)
                                                        (x12, w12) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11)
                                                        (x13, w13) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12)
                                                        (x14, w14) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13)
                                                        (x15, w15) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14)
                                                        (x16, w16) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15)
                                                        (x17, w17) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16)
                                                        (x18, w18) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17)
                                                        (x19, w19) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18)
                                                        (x20, w20) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18
                                                                             - w19)
                                                        (x21, w21) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18
                                                                             - w19
                                                                             - w20)
                                                        (x22, w22) <- genRandomTree
                                                                        (ub -
                                                                           1 - w0 - w1 - w2 - w3 -
                                                                             w4
                                                                             - w5
                                                                             - w6
                                                                             - w7
                                                                             - w8
                                                                             - w9
                                                                             - w10
                                                                             - w11
                                                                             - w12
                                                                             - w13
                                                                             - w14
                                                                             - w15
                                                                             - w16
                                                                             - w17
                                                                             - w18
                                                                             - w19
                                                                             - w20
                                                                             - w21)
                                                        return
                                                          (Node23 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22,
                                                           1 +
                                                             w22 + w21 + w20 + w19 + w18 + w17 + w16
                                                               + w15
                                                               + w14
                                                               + w13
                                                               + w12
                                                               + w11
                                                               + w10
                                                               + w9
                                                               + w8
                                                               + w7
                                                               + w6
                                                               + w5
                                                               + w4
                                                               + w3
                                                               + w2
                                                               + w1
                                                               + w0)

sampleTree :: RandomGen g => Int -> Int -> Rand g Tree
sampleTree lb ub
  = do sample <- runMaybeT (genRandomTree ub)
       case sample of
           Nothing -> sampleTree lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleTree lb ub

sampleTreeIO :: Int -> Int -> IO Tree
sampleTreeIO lb ub = evalRandIO (sampleTree lb ub)

instance Show Tree where
   show Node0 = "()"
   show (Node1 x0) = "(" ++ show x0 ++ ")"
   show (Node2 x0 x1) = "(" ++ show x0 ++ show x1 ++ ")"
   show (Node3 x0 x1 x2) = "(" ++ show x0 ++ show x1 ++ show x2 ++ ")"
   show (Node4 x0 x1 x2 x3) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ ")"
   show (Node5 x0 x1 x2 x3 x4) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ ")"
   show (Node6 x0 x1 x2 x3 x4 x5) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ ")"
   show (Node7 x0 x1 x2 x3 x4 x5 x6) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ ")"
   show (Node8 x0 x1 x2 x3 x4 x5 x6 x7) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ ")"
   show (Node9 x0 x1 x2 x3 x4 x5 x6 x7 x8) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ ")"
   show (Node10 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ ")"
   show (Node11 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ ")"
   show (Node12 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ ")"
   show (Node13 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ ")"
   show (Node14 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ ")"
   show (Node15 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ ")"
   show (Node16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ ")"
   show (Node17 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ ")"
   show (Node18 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ show x17 ++ ")"
   show (Node19 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ show x17 ++ show x18 ++ ")"
   show (Node20 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ show x17 ++ show x18 ++ show x19 ++ ")"
   show (Node21 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ show x17 ++ show x18 ++ show x19 ++ show x20 ++ ")"
   show (Node22 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ show x17 ++ show x18 ++ show x19 ++ show x20 ++ show x21 ++ ")"
   show (Node23 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ show x9 ++ show x10 ++ show x11 ++ show x12 ++ show x13 ++ show x14 ++ show x15 ++ show x16 ++ show x17 ++ show x18 ++ show x19 ++ show x20 ++ show x21 ++ show x22 ++ ")"
