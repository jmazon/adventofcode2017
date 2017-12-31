import Data.Array          (listArray,(!),indices,elems)
import Data.Graph          (components,graphFromEdges)
import Data.List.Split     (chunksOf)
import Data.Bits           (xor,testBit)
import Control.Applicative ((<|>))
import Control.Monad       (guard)
import qualified Data.IntSet as S

main = interact $ unlines . map show . flip map [part1,part2] .
                  flip ($) . disk . head . lines

rounds n rs = go 0 0 rs [0..n-1] where
  go p _    []   xs = rotate id ((-p) `mod` n) xs
  go p ss (r:rs) xs = go (p+r+ss) (ss+1) rs $
                      rotate id (ss `mod` n) $
                      rotate reverse r xs
rotate f r xs = right ++ f left where (left,right) = splitAt r xs
densify = map (foldl1 xor) . chunksOf 16
knot = densify . rounds 256 . concat . replicate 64 .
       (++ [17,31,73,47,23]) . map fromEnum

disk base = listArray ((0,0),(127,127)) $
            concatMap (\bs -> map (testBit bs) [7,6..0]) $
            concatMap (\i -> knot (base ++ "-" ++ show i)) [0..127]

part1 = length . filter id . elems
part2 d = length $ components g where
  (g,_,_) = graphFromEdges $ concatMap toEdges $ indices d
  toEdges (i,j) = do guard (d!(i,j))
                     let p = 128*i + j
                     [((),p,guard (i < 127 && d!(i+1,j)) *> pure (p+128) <|>
                            guard (j < 127 && d!(i,j+1)) *> pure (p+1))]
