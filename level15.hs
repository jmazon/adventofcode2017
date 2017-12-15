import Data.Bits
modulus = 2147483647 :: Int
genA = tail . iterate ((`mod` modulus) . (* 16807))
genB = tail . iterate ((`mod` modulus) . (* 48271))
judge a b = (a .&. 0x0000ffff) == (b .&. 0x0000ffff)
count n as bs = length $ filter (uncurry judge) $ take n $ zip as bs
multiplesOf n = filter ((== 0) . (`mod` n))
part1 = count 40000000 (genA startA) (genB startB)
part2 = count 5000000 (multiplesOf 4 (genA startA))
                      (multiplesOf 8 (genB startB))
main = print part1 >> print part2

(startA,startB) = (65,8921)

