-- Converted from puzzle input:
start = A
diag = 12172063

transition A False = (True,right,B)
transition A True  = (False,left,C)
transition B False = (True,left,A)
transition B True  = (True,left,D)
transition C False = (True,right,D)
transition C True  = (False,right,C)
transition D False = (False,left,B)
transition D True  = (False,right,E)
transition E False = (True,right,C)
transition E True  = (True,left,F)
transition F False = (True,left,E)
transition F True  = (True,right,A)

-- Generic code below:
data S = A | B | C | D | E | F
data M = M { mState :: !S, mLeft :: [Bool], mFocus :: !Bool, mRight :: [Bool] }

m0 = M start [] False []
left m w = case mLeft m of
  [] -> m { mFocus = False, mRight = w : mRight m }
  (b:bs) -> m { mLeft = bs, mFocus = b, mRight = w : mRight m }
right m w = case mRight m of
  [] -> m { mFocus = False, mLeft = w : mLeft m }
  (b:bs) -> m { mRight = bs, mFocus = b, mLeft = w : mLeft m }

step m = let (wr,mv,st) = transition (mState m) (mFocus m)
         in mv m { mState = st } wr

checksum m = length (filter id (mFocus m : mLeft m ++ mRight m))

main = print $ checksum $ foldr1 seq $ take (diag+1) $ iterate step m0

