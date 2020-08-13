import Math.GenBase.Recur
import Math.GenBase.Base
import Seq

bad_descend a = iterate (frombasef a . read . show) a
descend a = iterate (tobasef a . frombasef a) . read $ show a

ascend a = iterate (flip frombasef . read $ show a) a

descendl :: [Integer] -> Integer
descendl = foldl1 (\a -> frombasei a . map fromIntegral . fVect . read . show)

descendr :: [Integer] -> Integer
descendr = foldr1 (\a -> frombasei a . map fromIntegral . fVect . read . show)

descendfl :: [Double] -> Double
descendfl = foldl1 (\a -> frombasef a . read . show)

descendfr :: [Double] -> Double
descendfr = foldr1 (\a -> frombasef a . read . show)

seqMod p = map (`mod` p) . linseq . Literal

printList :: Show a => [a] -> IO ()
printList = mapM_ (putStrLn . show)

