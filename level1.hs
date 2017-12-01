import Data.Char (isDigit,digitToInt)
entry = map digitToInt . filter isDigit <$> readFile "level1.in"
solve modifier captcha = sum $ map fst $ filter (uncurry (==)) $ zipWith (,) captcha (modifier captcha)
part1 captcha = tail captcha ++ [head captcha]
part2 captcha = r ++ l
  where (l,r) = splitAt (length captcha `div` 2) captcha

-- solve part1 <$> entry
-- solve part2 <$> entry
