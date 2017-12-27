import Data.List.Split (chunksOf)
import Data.Bits       (xor)
import Text.Printf     (printf)

main = interact $ unlines . flip map [part1,part2] . flip ($) . head . lines

readAsList i = read $ "[" ++ i ++ "]"
readAsAscii = map fromEnum

knot n rs = go 0 0 rs [0..n-1] where
  go p _    []   xs = rotate id ((-p) `mod` n) xs
  go p ss (r:rs) xs = go (p+r+ss) (ss+1) rs $
                      rotate id (ss `mod` n) $
                      rotate reverse r xs
rotate f r xs = right ++ f left where (left,right) = splitAt r xs

densify = map (foldl1 xor) . chunksOf 16
hexify = concatMap (printf "%02x")

part1 = show . product . take 2 . knot 256 . readAsList
part2 = hexify . densify . knot 256 .
        concat . replicate 64 . (++ [17,31,73,47,23]) . readAsAscii
