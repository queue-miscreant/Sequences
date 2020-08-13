import Data.Complex
import Seq

avg xs = (*rec) $ sum xs
  where rec = recip $ fromIntegral $ length xs

harmonic xs = (*rec) . recip . sum . map recip $ xs
  where rec = fromIntegral $ length xs

--TODO use unfold
--
--util function to iteratively generate a list from results
--nothing about this implies a particular "zero"
--scan2 :: Eq a => a -> (a -> (b,a)) -> a -> [b]
--scan2 z f x | x == z    = []
--            | otherwise = fist:scan2 z f sound
--  where (fist, sound) = f x

scan2' = scan2 []
--semi-stable geometric mean that doesn't use too many numbers at once
stableGeometric xs = product $ map ((**rec) . product) $ scan2' (splitAt 10) xs
  where rec = recip $ fromIntegral $ length xs

agm a b = map fst $ iterate (\(x,y) -> (arith x y, geo x y)) (a,b)
  where arith a b = (/2) $ a + b
        geo a b   = sqrt $ a * b

agm1i = realPart $ flip (!!) 10 $ agm (1 :+ 0) (0 :+ 1) 
