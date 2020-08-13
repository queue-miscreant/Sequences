import Math.Sequence.Sequence (succRat, converge)

--look-and-say sequence generator from a particular seed
--i.e. "1" -> "11" -> "21"...
rleseq = iterate (unrle "" . rle [] '\0' 0)
  where unrle acc (('\0', 0):xs)          = acc
        unrle acc ((x, y):xs)             = unrle ((show y) ++ x:acc) xs
        rle acc lastchar charcount ""     = (lastchar, charcount):acc
        rle acc lastchar charcount (x:xs) | x /= lastchar = rle ((lastchar, charcount):acc) x 1 xs
                                          | otherwise     = rle acc lastchar (charcount+1) xs
rleseq' = rleseq "1"

--conway's constant; the root to a 71st degree polynomial
--note that this converges very slowly; even `converge` in the
--ratio method does not produce 4 correct digits
conwayConstant = converge 1e-4 $ succRat $ map (fromIntegral . length) rleseq'
