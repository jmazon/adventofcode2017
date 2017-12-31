import Data.Char     (isDigit,digitToInt)
import Control.Arrow ((&&&))
main = interact $ show . (solve part1 &&& solve part2) . entry
entry = map digitToInt . filter isDigit
solve modifier captcha = sum $ map fst $ filter (uncurry (==)) $
                         zip captcha (modifier captcha)
part1 captcha = tail captcha ++ [head captcha]
part2 captcha = r ++ l where (l,r) = splitAt (length captcha `div` 2) captcha
