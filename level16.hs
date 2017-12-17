import Data.Char     (isDigit)
import Data.List     (partition,elemIndex)
import Control.Arrow ((&&&))

main = interact $ show . (part1 &&& part2) . parse

parse = words . map commaToSpace where commaToSpace ',' = ' '
                                       commaToSpace  x  =  x

dance xs ('s':s) = r ++ l where (l,r) = splitAt (length xs - read s) xs
dance xs ('x':ds) = let (a,(_:b)) = break (not . isDigit) ds
                    in dance xs ['p',xs !! (read a),'/',xs !! (read b)]
dance xs ['p',a,_,b] = map f xs where
  f c = if c == a then b else if c == b then a else c
dance _ o = error o

p0 = ['a'..'p']
part1 = foldl dance p0
part2 ds = pb where
  (progPerms,posPerms) = partition ((== 'p') . head) ds
  poss = foldl dance p0 posPerms
  prgs = foldl dance p0 progPerms

  posOrbits = map (orbit poss) [0..15]
  prgOrbits = map (orbit prgs) [0..15]

  orbit ps = unCycle . iterate go where
    go i = i' where Just i' = elemIndex (ps !! i) p0

  applyOrbit is = is !! (1000000000 `mod` length is)
  prgb = map (p0 !!) $ map applyOrbit prgOrbits
  pb = map (prgb !!) $ map applyOrbit posOrbits

unCycle = go [] where
  go acc (x:xs) | x `elem` acc = []
                | otherwise    = x : go (x:acc) xs
