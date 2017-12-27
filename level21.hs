import Data.Bits
import Data.List
import Data.Array
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Arrow
    
main = do
  m <- parse <$> getContents
  print $ solve m 5
  print $ solve m 18

solve m n = length $ filter id $ elems $ iterate (step m) p0 !! n
p0 = readPat ".#./..#/###"

step m g | even s = array ((0,0),(s2-1,s2-1)) $
                    concatMap expand2 indices2
         | otherwise = array ((0,0),(s3-1,s3-1)) $
                       concatMap expand3 indices3
  where
    s = snd (snd (bounds g)) + 1
    s2 = 3 * s `div` 2
    s3 = 4 * s `div` 3
    indices2 = (,) <$> [0..s `div` 2-1] <*> [0..s `div` 2-1]
    indices3 = (,) <$> [0..s `div` 3-1] <*> [0..s `div` 3-1]
    expand2 (i,j) = let k = bits2key $ map ((g!) . offset (2*i,2*j))
                              [(0,0),(0,1),(1,0),(1,1)]
                        p = m M.! k
                    in [ ((3*i+ii,3*j+jj),b) | ((ii,jj),b) <- assocs p ]
    expand3 (i,j) = let k = bits2key $ map ((g!) . offset (3*i,3*j))
                              [ (0,0), (0,1), (0,2)
                              , (1,0), (1,1), (1,2)
                              , (2,0), (2,1), (2,2) ]
                        p = m M.! k
                    in [ ((4*i+ii,4*j+jj),b) | ((ii,jj),b) <- assocs p ]
    offset (a,b) (c,d) = (a+c,b+d)

data Key = Pat2 !Int | Pat3 !Int deriving (Eq,Ord)
parse = M.fromList . concatMap rule . lines where
  rule l = let [pat,_,out] = words l
           in map (flip (,) (readPat out)) $
              map (bits2key . concat) $
              map ($ pat2bitss pat) transforms
  transforms = (.) <$> [id,transpose] <*>
              ((.) <$> [id,reverse] <*> [id,map reverse])
readPat l = listArray bs $ map ((== '#') . (l !!)) is where
  (bs,is) | length l == 11 = (((0,0),(2,2)),[0,1,2, 4,5,6, 8,9,10])
          | length l == 19 = (((0,0),(3,3)),[0,1,2,3,     5,6,7,8
                                            ,10,11,12,13, 15,16,17,18])
pat2bitss = map (map (== '#')) . words . map unSlash where
  unSlash '/' = ' '
  unSlash  x  =  x
bits2key = go 0 0 where
  go 4 a [] = Pat2 a
  go 9 a [] = Pat3 a
  go n a (b:bs) = go (n+1) (if b then setBit a n else a) bs
