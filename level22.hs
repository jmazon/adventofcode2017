{-# LANGUAGE LambdaCase #-}
import qualified Data.IntMap.Strict as M
import Control.Arrow

-- dirty but simple, shaves like 30% time off
encode (i,j) = ((i + 2^31) * 2^32) + j + 2^31

main = interact $ show . (solve part1 10000 &&& solve part2 10000000) . parse

data Cell = Clean | Weakened | Infected | Flagged

parse i = M.fromList [ (encode (i,j),Infected)
                     | (j, l ) <- zip [-h `div` 2..] ls
                     , (i,'#') <- zip [-w `div` 2..] l ]
  where ls = lines i
        h = length ls
        w = length (head ls)

solve part n g0 = fst $ last {- foldr1 seq -} $ take (n+1) $
                  iterate step (0,((0,0),(0,-1),g0)) where
  step (ni,(p@(i,j),d,g)) = ni' `seq`
                            (ni',(p',d',g'))
    where (d'@(di',dj'),next) = part d $ M.findWithDefault Clean p_ g
          p' = (i+di',j+dj')
          g' = M.insert p_ next g
          p_ = encode p
          ni' = case next of Infected -> ni + 1
                             _        -> ni
  
part1 (di,dj) = \case Clean    -> (( dj,-di),Infected)
                      Infected -> ((-dj, di),Clean)

part2 (di,dj) = \case Clean    -> (( dj,-di),Weakened)
                      Weakened -> (( di, dj),Infected)
                      Infected -> ((-dj, di),Flagged)
                      Flagged  -> ((-di,-dj),Clean)
