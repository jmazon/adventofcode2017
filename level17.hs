import Control.Arrow ((&&&))

part1 s = go 1 [0] where
    go 2018 xs = xs !! 1
    go n xs = go (n+1) (n : r ++ l) where (l,r) = splitAt ((s+1) `mod` n) xs

part2 s = go 1 0 undefined where
    go 50000001 i a = a
    go n i a = go (n+1) i' $! if i' == 0 then n else a
        where i' = (i + s+1) `mod` n

main = interact $ show . (part1 &&& part2) . read
