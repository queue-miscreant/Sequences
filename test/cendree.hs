--widened borrow of a particular repeated amount 
--i.e., for borrow2 2, the borrow is 22 = 100

borrow2 b | b == 1    = error "Cannot borrow 1: implies positional symbol zero disallowed"
          | otherwise = borrow' [] b
  where borrow' zs b (x:y:z:xs) | abs x >= b = zipUp ys zs
                                | otherwise  = borrow' (x:zs) b (y:z:xs)
                                  where ys   = borr:(y-x+borr):z+(x`quot`b):xs
                                        borr = x `rem` b
        zipUp = foldl (flip (:))

--degenerate `borrow2 1 `that remedies inadequacies in below for 1, i.e., the borrow is 11 = 100
--this system is VERY bad because we invoke the place value '0' to expand into
--borrow1' zs (x:y:z:xs)  | abs x > 1 = zipUp ys zs
--                        | otherwise = borrow1' (x:zs) (y:z:xs)
--                          where ys  = (signum x):(y-x+signum x):z+(x-1):xs

--truncate the adic expansion for a system where borrows are 2 wide and both b
truncadic n b = take n . (!! n) . iterate (borrow2 b)

--given an amount of digits `n`, a 2-wide borrow `b`, and a canonical representation of `b`
--construct integer multiples of `b`
evens n b = ([0]:) . map (take n) . iterate (addb n b)
  --add b to an adic expansion
  where addb n b = (!! n ) . iterate (borrow2 b) . (b:) . tail

-- adic2 = (!! 1) $ evens 200 2 $ 0:0:cycle [1,-1] 
-- adic4 = (!! 2) $ evens 200 2 $ 0:0:cycle [1,-1] 
-- adic6 = (!! 3) $ evens 200 2 $ 0:0:cycle [1,-1] 
-- adic8 = (!! 4) $ evens 200 2 $ 0:0:cycle [1,-1] 
-- adic8 == truncadic 200 2 $ (++repeat 0) $ map (*2) adic4
