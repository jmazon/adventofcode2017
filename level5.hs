{-# LANGUAGE FlexibleContexts #-}

import Data.Array.ST
import Control.Monad.ST
import Control.Arrow
main = interact $ show . (solve step1 &&& solve step2) . map read . lines

solve step xs = runST $ do
    a <- newListArray (0,n-1) xs :: ST s (STUArray s Int Int)
    go a 0 0
  where n = length xs
        go a i c | i >= n    = return c
                 | otherwise = do
          v <- readArray a i
          writeArray a i (step v)
          go a (i + v) $! c+1

step1 = succ
step2 i | i >= 3    = i - 1
        | otherwise = i + 1
