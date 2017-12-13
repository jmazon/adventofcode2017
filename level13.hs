import Control.Arrow
main = interact $ show . (part1 &&& part2) . map parseLine . lines
parseLine x = (read (init l),read r :: Int) where [l,r] = words x
part1 = sum . map (uncurry (*)) . filter catch
  where catch (d,r) = d `mod` (2*r-2) == 0
part2 fs = head $ filter (\w -> all (noCatch w) fs) [0..]
  where noCatch w (d,r) = (d+w) `mod` (2*r-2) /= 0
