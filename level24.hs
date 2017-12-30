{-# LANGUAGE FlexibleContexts #-}

import Data.Ord
import Data.Bits
import Data.List
import Data.Array
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Arrow

main = interact $ show . (solve snd &&& solve id) . parse

parse i = listArray (0,length ls - 1) $ map f ls where
  ls = lines i
  f l = (read a,read b) where (a,_:b) = break (== '/') l

solve f ps = evalState (go 0 bs0) M.empty where
  bs0 = 2^length (elems ps) - 1 :: Int
  go at bs = do
    mass <- M.lookup (at,bs) <$> get
    case mass of
      Just m -> return m
      Nothing -> do
        let candidates = [ (i,at') | i <- indices ps, testBit bs i
                                     , at' <- other (ps!i) at ]
        m <- fmap (maximumBy (comparing f) . ((0,0):)) $
             forM candidates $ \(i,at') ->
             ((+1) *** (at+at'+)) <$> go at' (clearBit bs i)
        modify (M.insert (at,bs) m)
        return m

other (a,b) c | a == c = [b]
              | b == c = [a]
              | otherwise = []
