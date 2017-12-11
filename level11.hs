import Data.Char     (toUpper)
import Data.Complex  (Complex((:+)))
import Control.Arrow ((&&&))
main = interact $ show . (last &&& maximum) . solve . parse
parse = read . ('[' :) . (++ "]") . map toUpper
solve = map dist . scanl1 (+) . map adv
data Dir = NE | N | NW | SW | S | SE deriving Read
adv NE =   1  :+ (-1)
adv N  =   0  :+ (-1)
adv NW = (-1) :+   0
adv SW = (-1) :+   1
adv S  =   0  :+   1
adv SE =   1  :+   0
dist (x :+ y) = round (abs x + abs y + abs (x+y)) `div` 2
