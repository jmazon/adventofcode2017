{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.Array

-- Specific: machine description converted from my puzzle input:
inStart = A
inDiag = 12172063
data S = A | B | C | D | E | F
inTransition A False = (True ,right,B)
inTransition A True  = (False,left ,C)
inTransition B False = (True ,left ,A)
inTransition B True  = (True ,left ,D)
inTransition C False = (True ,right,D)
inTransition C True  = (False,right,C)
inTransition D False = (False,left ,B)
inTransition D True  = (False,right,E)
inTransition E False = (True ,right,C)
inTransition E True  = (True ,left ,F)
inTransition F False = (True ,left ,E)
inTransition F True  = (True ,right,A)

-- Generic part of the code:
data M s = M { mState :: !s, mLeft :: [Bool], mFocus :: !Bool, mRight :: [Bool] }
m0 start = M start [] False []
left m w = case mLeft m of
  [] -> m { mFocus = False, mRight = w : mRight m }
  (b:bs) -> m { mLeft = bs, mFocus = b, mRight = w : mRight m }
right m w = case mRight m of
  [] -> m { mFocus = False, mLeft = w : mLeft m }
  (b:bs) -> m { mRight = bs, mFocus = b, mLeft = w : mLeft m }
step tr m = let (wr,mv,st) = tr (mState m) (mFocus m)
            in mv m { mState = st } wr
checksum m = length (filter id (mFocus m : mLeft m ++ mRight m))
solve (st,d,tr) = checksum $ foldr1 seq $ take (d+1) $ iterate (step tr) (m0 st)

main = do print $ solve (inStart,inDiag,inTransition)
          print . fmap solve . parse blueprint "(source)" =<< getContents

-- Generic machine description parser:
blueprint = do
  start <-         wrapped "Begin in state " upper "."
  diag <- read <$> wrapped "Perform a diagnostic checksum after "
                           (many digit) " steps."
  trs <- fmap concat $ many $ do
    endOfLine
    st <- wrapped "In state " upper ":"
    many $ do
      v  <- (== '1') <$> wrapped "  If the current value is " digit ":"
      v' <- (== '1') <$> wrapped "    - Write the value " digit "."
      d  <-              wrapped "    - Move one slot to the "
                                 (string "left" <|> string "right") "."
      st' <-             wrapped "    - Continue with state " upper "."
      return ((st,v),(v',if d == "left" then left else right,st'))
  let states = map (fst . fst) trs
      a = array ((minimum states,False),(maximum states,True)) trs
      transition = curry (a!)
  return (start,diag,transition)
wrapped pre p post = string pre *> p <* string post <* endOfLine
