
import Control.Arrow.Par
import Data.List (foldl')

foo :: Integer -> (Integer,Integer)
foo = applyPar $ arr f &&& arr g
  where
    sum' = foldl' (+) 0
    f n  = sum' [1 .. n]
    g n  = sum' [2 .. n]

main :: IO ()
main = print $ foo 200000000
