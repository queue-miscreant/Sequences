--module for stern-brocot-like functions
module Math.Sequence.Stern where

import Prelude
import GHC.Real

--stern-brocot rational number builder
--takes a sequence of [0,1], interpreting 0 as left (toward 0) and 1 as right (toward inf)
sternBrocot :: Integral a => [Int] -> Ratio a
sternBrocot = sternBrocot' (1 :% 1) (0 :% 1) (1 :% 0)
  where sternBrocot' (x :% y) (a :% b) (c :% d) []     = (x :% y)
        sternBrocot' (x :% y) (a :% b) (c :% d) (0:xs) = sternBrocot' ((x+a) :% (y+b)) (a :% b) (x :% y) xs
        sternBrocot' (x :% y) (a :% b) (c :% d) (1:xs) = sternBrocot' ((x+c) :% (y+d)) (x :% y) (c :% d) xs

--reverse-stern-brocot sequence builder
--transforms a rational number into a sequence of 0 (left) and 1 (right)
unSternBrocot :: Integral a => Ratio a -> [Int]
unSternBrocot = unSternBrocot' [] (1 :% 1) (0 :% 1) (1 :% 0)
  where unSternBrocot' _ _ _ _ (1 :% 0) = divZeroError
        unSternBrocot' acc here@(x :% y) (a :% b) (c :% d) frac = case (frac `compare` here) of
           EQ -> reverse acc
           LT -> unSternBrocot' (0:acc) ((x+a) :% (y+b)) (a :% b) here frac
           GT -> unSternBrocot' (1:acc) ((x+c) :% (y+d)) here (c :% d) frac

calkinWilf = calkinWilf' (1 :% 1)
  where calkinWilf' frac []         = frac
        calkinWilf' (a :% b) (x:xs) | x == 0    = calkinWilf' (a :% (a+b)) xs
                                    | otherwise = calkinWilf' ((a+b) :% b) xs

--construct rational number of continued fraction
--remember to use takes of infinite sequences
continued :: Integral a => [a] -> Ratio a
continued []     = 1 :% 0 --don't think about the infinity too much
continued (x:xs) = (x :% 1) + (recip $ continued xs)

--inverse of `continued`
--create continued fraction representation from rational number
uncontinued :: Integral a => Ratio a -> [a]
uncontinued (_ :% 0) = []
uncontinued (a :% b) = q:uncontinued (b :% r)
  where (q,r) = a `divMod` b

--continued fraction with additional control over the numerator
continued2 :: Integral a => [(a,a)] -> Ratio a
continued2 []         = 1 :% 0 
continued2 ((x,y):xs) = (x :% 1) + (y :% 1) / (continued2 xs)

--simple continued fraction for doubles
continuedf :: Floating a => [a] -> a
continuedf [x]    = x
continuedf (x:xs) = x + (recip $ continuedf xs)
