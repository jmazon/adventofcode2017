import qualified Data.Map.Strict as M

-- so I thought this was math
part1 n = go (n-1) 1 0 where
  go n r p | n <= p' = (r+abs (n `mod` s - hs))
           | otherwise = go (n-p') (r+1) p'
    where p' = p + 8
          s = p' `div` 4
          hs = s `div` 2

-- when really, it's bruteforce
part2 n = go (M.singleton (0,0) 1) (1,0) where
  go spiral (x,y)
    | next > n  = [next]
    | otherwise = next:go (M.insert (x,y) next spiral) (x+dx,y+dy)
    where
      next = sum $ map (\k -> M.findWithDefault 0 k spiral)
        [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1)
        ,(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]
      (dx,dy) | y > -x && y <  x = ( 0, 1)
              | y > -x && y >= x = (-1, 0)
              |           y >  x = ( 0,-1)
              | otherwise        = ( 1, 0)
