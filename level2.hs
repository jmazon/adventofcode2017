import Data.List (sort)
import Control.Arrow
main = interact $ show . (solve part1 &&& solve part2) . lines

solve f = sum . map (f . map read . words)
part1 = (-) <$> maximum <*> minimum
part2 = head . go . sort where
  go (x:xs) = ((`div` x) <$> filter ((== 0) . (`mod` x)) xs) ++ go xs
  go [] = []
