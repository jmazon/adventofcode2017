import           Data.List
import           Data.Ord      (comparing)
import           Data.Maybe    (catMaybes)
import           Data.Function (on)
import           Control.Arrow ((&&&))
import           Text.Parsec
import qualified Data.Map as M
import qualified Data.IntSet as S

data V = V { vX :: !Int, vY :: !Int, vZ :: !Int } deriving (Eq,Ord)
data P = P { pId :: !Int, pP :: !V, pV :: !V, pA :: !V } deriving Eq

main = interact $ show . (part1 &&& part2) . runParser (many part) 0 "<stdin>"

-- parsing

part = P <$> (getState <* modifyState succ) <*> vec <*> vec <*> vec
vec = try (skipMany (noneOf "<") <* char '<') *>
  (V <$> num <* char ',' <*> num <* char ',' <*> num <* char '>')
num = (read .) . (:) <$> option ' ' (char ' ' <|> char '-') <*> many digit

-- part1: sort by asymptote

part1 (Right ps) = pId $ flip minimumBy ps $
                   mconcat $ map (comparing . (n1 .)) [pA,pV,pP]
  where n1 (V x y z) = abs x + abs y + abs z

-- part2: generate all future collisions; resolve them in order

-- It seems simulation was all that's needed, as collisions for my
-- test input only occur on all integral times between 10 and 39, but
-- who'd have known.  So my code is quadratic in number of particles
-- and independent of the collisions' actual timing.

-- Also, note that the "Avoided collision" trace below nevers
-- triggers.  This means that no particle has a course who
-- theoretically collides with two others at distinct time/places.  So
-- it would be enough to just generate all future collisions, and not
-- resolve at all, just count the unimpacted particles.  (This is
-- actually how I solved it initially.)  It happens to work with my
-- test input, but it can't be excluded from the statement alone as
-- far as I can tell, so I'm keeping the complete logic.  No serious
-- slowdown, it's the collision generation that really takes time.

part2 (Right ps) = go is0 cs0 where
  is0 = S.fromList $ map pId ps
  cs0 = M.fromList $ map (fst . head &&& S.fromList . concatMap snd) $
        groupBy ((==) `on` fst) $ sort $
        concat [ collision p1 p2 | (p1:ps) <- tails ps, p2 <- ps ]
  go is cs = case M.minViewWithKey cs of
    Nothing -> S.size is
    Just ((r,isc),cs') -> go is' cs' where
      is' | S.size (S.intersection is isc) > 1 =
              -- trace ("Collision at " ++ show r ++ " between " ++ show isc) $
              S.difference is isc
          | otherwise = {- trace ("Avoided collision at " ++ show r) -} is

collision o1@(P i1 p1 v1 a1) o2@(P i2 p2 v2 a2) =
    case concatMap verify ts of [r] -> [(r,[i1,i2])]
                                [] -> []
 where
  partialCollide f = solve (fromIntegral (f a2 - f a1) / 2)
                           (fromIntegral (2*f v2 + f a2 - 2*f v1 - f a1) / 2)
                           (fromIntegral (f p2 - f p1))
  ts = nub $ filter (>= 0) $ foldl1 intersect $ map (map round) $
       catMaybes $ map partialCollide [vX,vY,vZ]
  -- solve works on Double, so verify ensures only integral solutions are kept
  verify t | p1 == p2 = [(t,p1)]
           | otherwise = []
    where p1 = pP (advance t o1)
          p2 = pP (advance t o2)

-- Where's that particle at time t?
advance t (P i p v a) = P i p' v' a where
  v' = v +. (t*.a)
  p' = p +. (t*.v) +. ((t*(t+1)`div`2)*.a)
  (V a b c) +. (V d e f) = V (a+d) (b+e) (c+f)
  l *. (V a b c) = V (l*a) (l*b) (l*c)

-- Generic solver for ax² + bx + c = 0
-- Returns Just [sol1,sol2...] for multiple solutions,
--         Just [] for no solution,
--         Nothing for degenerate equation (any x is solution)
solve :: Double -> Double -> Double -> Maybe [Double]
solve a b c | abs a > epsilon = if delta > epsilon
                                then Just [ (-b-sqrt delta)/(2*a)
                                          , (-b+sqrt delta)/(2*a) ]
                                else Just [-b/(2*a)]
            | abs b > epsilon = Just [-c/b]
            | abs c > epsilon = Just []
            | otherwise = Nothing
  where delta = b*b -4*a*c
        epsilon = 1e-6
