import Data.List (sort,nub)
import Control.Arrow
main = interact $ show . (solve part1 &&& solve part2) . lines
solve f = length . filter (valid f)
valid part l = nub ws == ws where ws = part <$> words l
part1 = id
part2 = sort
