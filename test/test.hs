import Math.GenBase.Recur
import Math.GenBase.Base

seqMod p = map (`mod` p) . linseq . Literal

printList :: Show a => [a] -> IO ()
printList = mapM_ (putStrLn . show)
