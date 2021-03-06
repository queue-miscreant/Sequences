module Math.Sequence.Sequence where

import Prelude

--most of this file is functions I assembled for analysis of certain sequences

--ratio between successive entries of a list
succRat xs = zipWith (/) (tail xs) xs
--first differences between successive entries of a list
firstDiff xs = zipWith (-) (tail xs) xs
--list of partial sums of a sequence
partialSum = scanl1 (+)
--infinite list of takes of a sequence
slices = (map take [1..] <*>) . pure

--run lengths of a sequence
runs [] = [0]
runs (x:xs) = runs' x 1 xs
  where runs' cur len []     = [len]
        runs' cur len (y:ys) | y == cur  = runs' cur (len+1) ys
                             | otherwise = len:(runs' y 1 ys)
--un-run lengths of a sequence, generated by 0 and 1
unruns = concat . zipWith (flip replicate) (cycle [0,1])

--creates words out of sequences of Eqs, with alphabet starting from an enumerable `a`
toAlpha :: (Enum a, Eq b) => a -> [b] -> [a]
toAlpha a = toAlpha' [a..] []
  where toAlpha' alpha morph []            = []
        toAlpha' alpha@(a:as) morph (x:xs) = case lookup x morph of
          Just y  -> y:toAlpha' alpha morph xs
          Nothing -> a:toAlpha' as ((x,a):morph) xs

--size of the alphabet generated by the above
alphaSize :: (Eq a) => [a] -> Int
alphaSize = foldl1 max . toAlpha 1

--try to find the period of sequence xs, trying to match `t` patterns ahead
period t (x:xs) = period' [x] 1 t xs
  where period' bs n p xs@(x:xxs)
          | p == 0      = n
          | sb == first = period' bs n (p-1) last
          | p < t       = period' (x:bs') (n'+1) t xxs
          | otherwise   = period' (x:bs)  (n+1)  t xxs
            where (first, last) = splitAt n xs
                  sb            = reverse bs
                  bs'           = take n' $ cycle bs
                  n'            = n*(t-p)

--find point in sequence where absolute difference between successive entries
--is less then `prec`
converge prec = fst . head . dropWhile ((>prec) . abs . uncurry (-)) . (zip <*> tail)

--for f, a function of a single integer that generates better approximations for
--larger inputs, find the minimum number of iterations such that the difference between
--terms is smaller than floating point precision
precision f = fst $ head $ drops $ [(f n, n) | n <- [1..]]
  where drops = dropWhile (\((x,_),(y,_)) -> x /= y) . (zip <*> tail)

--number of terms for which two lists are equal
numCoincide :: Eq a => [a] -> [a] -> Int
numCoincide = ((length . takeWhile (uncurry (==)) ).) . zip

--position of terms in a sequence that are nonzero
nonZeroPos = map fst . filter ((/=0) . snd) . zip [0..]


--non-involutive binomial transform (and its inverse)
--produces pascal's triangle row by addition

bin' f s = bin'' 0 [1]
  where bin'' a row = (sum $ zipWith (*) s $ f a row):bin'' (a+1) (nextrow row)
        nextrow xs = (zipWith (+) <*> tail) $ 0:xs ++ [0]

--ignore the row number argument, just return the row
binomtf = bin' seq
--alternate between - and + times the row
invbinomtf = bin' (\a -> zipWith (*) $ drop (a `mod` 2) $ cycle [1,-1])

--series transform equivalent to, for an ordinary generating function F(x)
--producing a series with the generating function 1/(1-xF(x))
--for a Lucas sequence lacking an initial 0 (i.e., the x in the numerator putting the 0 back), this
--produces a Lucas sequence with initial 0 replaced by a 1 and the first argument incremented
--(i.e., tail L(a,b) -> 1:tail L(a+1,b))
gfRecip s = 1:inverse' [1]
  where inverse' xs = next:inverse' (next:xs)
         where next = sum $ zipWith (*) xs s
