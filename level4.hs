import Data.List
main = interact $ show . length . filter (valid part2) . lines
valid part l = nub ws == ws where ws = part <$> words l
part1 = id
part2 = sort
