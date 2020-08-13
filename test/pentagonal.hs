--I've given up trying to figure out what I was doing beyond generating the pentagonal numbers
import Data.List

tri = [n*(n+1) `div` 2 | n <- [1..]]
genPent = [n `div` 3 | n <- tri, (n `rem` 3 == 0)]
pent = pent' True genPent
  where pent' b (x:xs) | b         = x:pent' False xs
                       | otherwise = pent' True xs

split xs = split' 0 xs [] []
  where split' _ [] as bs = (reverse as, reverse bs)
        split' 0 (x:xs) as bs = split' 1 xs (x:as) bs
        split' 1 (x:xs) as bs = split' 0 xs as (x:bs)

pentaZip f n a | a < 0     = zipWith f (drop (-a) pos) neg
               | otherwise = zipWith f (drop a neg) pos
  where (pos, neg) = split (take n pent)

pentaDelta = pentaZip (-)
pentaSum = pentaZip (+)

delta xs = zipWith (-) (tail xs) xs


sortFunc f = sortBy (\x y -> f x `compare` f y)

stuff = map (head . snd) $ sortFunc (abs . fst) $ (\x -> map (\n -> (n, delta $ pentaDelta x n)) [(-10)..10]) 30
