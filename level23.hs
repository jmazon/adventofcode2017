{-# LANGUAGE FlexibleContexts #-}

import Data.Char
import Control.Monad.RWS
import qualified Data.Map as M

main = do
  prg <- parse <$> getContents
  print $ snd $ evalRWS prg () M.empty
  -- part 2 with "pencil and paper"

-- Simplified from day 18:
parse i = head is where
  is = zipWith decode [0..] . map words $ lines i
  decode ip ["set",x,y] = alu x   const  y >> adv ip
  decode ip ["sub",x,y] = alu x subtract y >> adv ip
  decode ip ["mul",x,y] = alu x    (*)   y >> tell (Sum 1) >> adv ip
  decode ip ["jnz",x,y] = do
    prev <- load x <$> get
    if prev /= 0 then jmp . (+ ip) . load y =<< get else adv ip
  adv ip = jmp (ip+1)
  jmp ip | ip < 0 || ip >= length is = return ()
         | otherwise = is !! ip

  alu x op y = modify $ \ds ->
    let v' = op (load y ds) (M.findWithDefault 0 (head x) ds)
    in M.insert (head x) v' ds

  load v | isAlpha (head v) = M.findWithDefault 0 (head v)
         | otherwise = const (read v)
