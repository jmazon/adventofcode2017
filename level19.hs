import Data.List     (elemIndices)
import Control.Arrow ((&&&))
main = interact $ show . (concat &&& length) . uncurry solve . parse
parse i = (e,ls) where ls  = lines i
                       [e] = elemIndices '|' (head ls)
solve e g = go (0,e) (1,0) where
  go p d | g!p == ' '               =  []
         | g!p == '|' || g!p == '-' =  []   : go (adv p d ) d
         | g!p == '+'               =  []   : go (adv p d') d'
         | otherwise                = [g!p] : go (adv p d ) d
    where a!(i,j) = a !! i !! j
          (di,dj) = d
          (d':_) = filter ((/= ' ') . (g!) . adv p) [(dj,-di),(-dj,di)]
  adv (i,j) (di,dj) = (i+di,j+dj)
