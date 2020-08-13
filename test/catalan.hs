import Math.GenBase.Base (seqcount)

--the catalan numbers; an important integer sequence with number theoretical applications
catalan :: [Integer]
catalan = map (\n -> product [2*n,2*n-1..n+1] `div` (product [1..n] * (n+1))) [0..]
--catalan-quaternary expansions of natural numbers
basecat = seqcount catalan
