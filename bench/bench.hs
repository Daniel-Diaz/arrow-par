
import Prelude hiding ((.),id)
import Criterion.Main
import Control.Arrow.Par
import Data.List (foldl',partition)
import Data.Maybe (maybeToList)

-- FUNCTIONS

sum' :: [Integer] -> Integer
sum' = foldl' (+) 0

sum1 :: Integer -> (Integer,Integer)
sum1 = arr f &&& arr g
  where
   f n = sum' [1 .. n]
   g n = sum' [2 .. n]

sum2 :: Integer -> (Integer,Integer)
sum2 = applyPar $ arr f &&& arr g
  where
   f n = sum' [1 .. n]
   g n = sum' [2 .. n]

qsort1 :: Ord a => [a] -> [a]
qsort1 (x:xs) =
  let f = arr (\(l,r) -> l ++ x : r) . (arr qsort1 *** arr qsort1) . arr (partition (<x))
  in  f xs
qsort1 [] = []

qsort2 :: Ord a => [a] -> [a]
qsort2 (x:xs) =
  let f = arr (\(l,r) -> l ++ x : r) . (arr qsort2 *** arr qsort2) . arr (partition (<x))
  in  applyPar f xs
qsort2 [] = []

testlist :: [Int]
testlist = [2000,1999 .. 1]

-- BENCHMARKS

main :: IO ()
main = defaultMain [
    bgroup "sum"   [ bench "original" $ nf sum1 10000000
                   , bench "parallel" $ nf sum2 10000000
                     ]
  , bgroup "qsort" [ bench "original" $ nf qsort1 testlist
                   , bench "parallel" $ nf qsort2 testlist
                     ]
  ]
