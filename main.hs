module Main where

import Data.List (subsequences, nub)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Set (Set)
import qualified Data.Set as Set
import Queue (Queue)
import qualified Queue as Q

data Ball = Solid
          | Stripe
          | Black deriving (Eq, Ord, Show)

type Comb = Array Int Ball
type Moves = Int

swaps :: [(Int, Int)]
swaps = map (\x -> (x !! 0, x !! 1)) .
        filter (\x -> length x == 2) .
        subsequences $ [0..14]

-- Helper function
step :: [(Int, Int)] -> Comb -> [Comb]
step swaps comb = map (doSwap comb)
                $ swaps

doSwap :: Comb -> (Int, Int) -> Comb
doSwap comb (i, j) = comb A.// [(i, comb A.! j), (j, comb A.! i)]

rotateCCW :: Comb -> Comb
rotateCCW c = c A.// [ (0,  c A.! 10)
                     , (1,  c A.! 11)
                     , (2,  c A.! 6 )
                     , (3,  c A.! 12)
                     , (4,  c A.! 7 )
                     , (5,  c A.! 3 )
                     , (6,  c A.! 13)
                     , (7,  c A.! 8 )
                     , (8,  c A.! 4 )
                     , (9,  c A.! 1 )
                     , (10, c A.! 14)
                     , (11, c A.! 9 )
                     , (12, c A.! 5 )
                     , (13, c A.! 2 )
                     , (14, c A.! 0 )
                     ]

rotateCW :: Comb -> Comb
rotateCW = rotateCCW . rotateCCW

baseComb :: Comb
baseComb = A.listArray (0, 14) [ Solid
                               , Solid, Stripe
                               , Stripe, Black, Solid
                               , Stripe, Solid, Stripe, Stripe
                               , Stripe, Solid, Stripe, Solid, Solid
                               ]

bsf :: Set Comb -> Queue (Comb, Moves) -> (Comb, Moves)
bsf visited queue
    | Q.length queue == 1 && null next = (current, m)
    | otherwise = bsf visited' queue''
    where
        ((current, m), queue') = Q.pop queue
        next = filter (\x -> Set.notMember x visited)
             . nub
             . (:) (rotateCW current)
             . (:) (rotateCCW current)
             . step swaps 
             $ current
        visited' = foldl (\set x -> Set.insert x set) visited $ next
        queue'' = foldl (\q x -> Q.push q x) queue'
               . map (\x -> (x, m+1))
               $ next

main = do
    print baseComb
    print . bsf (Set.singleton baseComb) 
          $ Q.singleton (baseComb, 0)
