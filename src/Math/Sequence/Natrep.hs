--module for manipulating representations of natural numbers
module Math.Sequence.Natrep where

import Prelude

--if xs is some representation of natural numbers, then
--returns those natural numbers `x` for which `f x` is `True`
--note that "notSatisfying" is not equivalent to applying (not . f)
satisfying xs f = map fst $ filter (f . snd) $ zip [0..] xs

--if xs is an increasing list of integers, then missing xs
--is those integers which do not appear in xs
missing = missing' [0..]
  where missing' xs      []        = xs
        missing' (x:xs) ys@(y:yys) | x /= y    = x:(missing' xs ys)
                                   | otherwise = missing' xs yys

--numbers lost in a particular (monotonic) transformation
--i.e., (list) `lost` (\n -> ...)
lost = flip $ (missing .) . map
