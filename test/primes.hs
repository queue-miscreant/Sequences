--prime factorizations for integers including 1 and 0
factorized :: [[Int]]
factorized = [0]:[1]:factorized' [] [2..]
  where factorized' ps (n:ns) 
         | null factor  = [n]:factorized' (n:ps) ns
         | otherwise    = factor:factorized' ps ns
           where factor = factor' n $ reverse ps
                 factor' n []        = []
                 factor' 1 _         = []
                 factor' n ps@(p:pp) | rem == 0  = p:factor' div ps
                                     | otherwise = factor' n pp
                    where (div,rem) = divMod n p
