import Data.List (sort)
main = interact $ show . sum . map (part2 . map read . words) . lines
part1,part2 :: [Int] -> Int
part1 = (-) <$> maximum <*> minimum
part2 = head . go . sort where
  go (x:xs) = ((`div` x) <$> filter ((== 0) . (`mod` x)) xs) ++ go xs
  go [] = []
