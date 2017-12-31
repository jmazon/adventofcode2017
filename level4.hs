import Data.List (sort,nub)
import Control.Arrow ((&&&))
main = interact $ show . (solve id &&& solve sort) . lines
solve f = length . filter (valid f)
valid part l = nub ws == ws where ws = part <$> words l
