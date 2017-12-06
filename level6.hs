import Data.Ord
import Data.Foldable
import Data.Array
import qualified Data.Map.Strict as M
main = interact $ show . solve . parse
parse l = listArray (0,n-1) xs where
  xs = map read (words l)
  n = length xs
solve = go 0 M.empty . iterate step where
  go i m (v:vs) | Just i' <- M.lookup v m = (i,i-i')
                | otherwise               = go (i+1) (M.insert v i m) vs
step a = accum (+) a ((m,-v) : map (\i -> ((m+i) `mod` n,1)) [1..v]) where
  (m,v) = maximumBy (comparing snd `mappend` comparing (Down . fst)) (assocs a)
  n = snd (bounds a) + 1
