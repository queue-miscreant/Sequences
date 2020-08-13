import Seq
import Base
import Natrep

catalan = map (\n -> product [2*n,2*n-1..n+1] `div` (product [1..n] * (n+1))) [0..]
basecat = seqcount catalan
