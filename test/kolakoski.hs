import Math.Sequence.Sequence
import Math.Sequence.Stern

--interpret a boolean sequence as a binary number
binseq = foldl (\x y -> 2*x + y) 0 
revbinseq = sum . zipWith (*) (iterate (*2) 1)

--interpret a sequence of -1, 0, and 1 as a balanced ternary number
tern :: Integral a => [a] -> a
tern = foldl (\x y -> 3*x + y) 0 
revtern = sum . zipWith (*) (iterate (*3) 1)

--kolakoski sequence; uses itself as a sort of instruction tape
--kolakoski = 1:2:(genK (drop 2 kolakoski) 2 True)
--  where genK instruct   last True  = last:(genK instruct last False)
--        genK (run:runs) last False = next:(genK runs next (run == 2))
--          where next = if last == 1 then 2 else 1
kolakoski = 1:2: drop 2 (concat . zipWith replicate kolakoski $ cycle [1,2])

--kolakoski sequence starting with 3 3's and continuing, cycling between 1, 2, and 3
kola3 = 3:3:3: drop 3 (concat . zipWith replicate kola3 . cycle $ [3,1,2])
--kolakoski sequence starting with 3 3's and continuing, cycling only between 1 and 2
kola3' = 3:3:3: drop 3 (concat . zipWith replicate kola3' . cycle $ [2,1])

--sequence OEIS A329356
kola2 = map binseq $ slices $ map (fromEnum . (==1)) kolakoski

--generate a sequence based on a (boolean) function of adjacent pairs of the translated sequence
transformKola f = slices $ (\xs -> zipWith f xs (tail xs)) kolakoski
kolaBits = map (binseq . map fromEnum) . transformKola

--certain equalities match cellular automata for the first few terms
--diverges from rules 91 and 25 after 8 terms (OEIS A267045, A266446)
kolaLt = kolaBits (<)

--the kolakoski sequence initially appears to obey equality cycle (<=, <=, >)
kolaLe = kolaBits (<=)

--matches no known sequence in OEIS
kolaEq = kolaBits (==)

--first difference substrings interpred as balanced ternary digits
--matches no known sequence in OEIS
ternseq = map tern $ slices $ firstDiff kolakoski

--sequences of rationals that both approach an apparent constant
kolaSternBrocot = map sternBrocot $ slices $ map (2-) kolakoski
kolaContinued   = map continued $ slices kolakoski

--sequence generated from back-and-forth interpretations of continued fractions
--and stern-brocot representations
--sequences seem to converge to the above constant, diverge (to 1 % 0),
--or cycle (as with 3/2 = [1; 1, 1], but not 5/3 = [1; 1, 1, 1])
--Note that continued [n, 1] = continued [n+1] = (n+1 % 1)
--
--backandforth [n]    diverges for n=1-3, cycles about 1.5 for n=4, and converges for n>=5
--backandforth [0, n] diverges for n=1 and 2 and converges for n>=3
--backandforth [1, n] diverges for n=1, cycles for n=2, and converges for n>=3
--backandforth [2, n] diverges for n=1 and converges for n>=2
--backandforth [3, n] cycles for n=1 and converges for n>=2
--backandforth [m, n] m >= 4 converges for all n>=1
--backandforth [1, 1, n] cycles for n=1 and converges for n>=2
--backandforth [1, m, n] m >= 2 converges for n>=1
--backandforth [1, 1, m, n] converges for n>=1
--backandforth [2, 1, n] converges for n>=1
--backandforth [3, 1, n] converges for n>=1
backandforth  = map continued . iterate (map (2-) . unSternBrocot . continued)
backandforth' = map continued . iterate (map (+1) . unSternBrocot . continued)

--a - 1/b (in the limit) is 1, by definition of the sequence and continued fraction
a = precision $ (!!) $ map fromRational $ backandforth [5]
b = precision $ (!!) $ map fromRational $ backandforth' [5]
