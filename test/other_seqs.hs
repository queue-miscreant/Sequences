import Math.Sequence.Sequence
import Math.Sequence.Stern

--reversed primes up to m; not particularly efficient since it checks the largest
--primes first
primes m = foldl (\p n -> if and $ map ((/=0) . (n `rem`)) p then n:p else p) [] [2..m]

--list of 2-tuples of the number of correct digits of the continued fraction 
--expects a number for comparison n and a partial fraction sequence
continuedApprox n = map (\x -> (comparedec $ fractslice x, x))
  where comparedec = (+(-1)) . numCoincide (show n) . show
        fractslice = fromRational . continued

--list of correct decimal digits from continued fraction expansions of pi
--simple monotonically increases
pisimple  = continuedApprox pi $ slices $ uncontinued $ toRational pi
picomplex = continuedApprox pi $ map (uncontinued . pifrac) [1..]

--slowly converging continued fraction of pi
pifrac i = continued2 $ take i $ zip (3:(repeat 6)) [(2*n-1)^2 | n <- [1..]]

--alternate description of the catalan numbers as the limit of
--the continued application of this sequence transform
catalan n = take (n+1) $ iterate (gfRecip .) id !! n $ [1]
