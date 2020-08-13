recaman = 0:recaman' 1 recaman
  where recaman' d (x:xs) | back      = (x-d):recaman' (d+1) xs
                          | otherwise = (x+d):recaman' (d+1) xs where {
    back = (x>d) && (x-d) `notElem` take d recaman}

main = putStrLn $ show $ take 20 recaman
