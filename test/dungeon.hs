import Math.GenBase.Base

--bad_descend a = iterate (frombasef a . read . show) a
descend a = iterate (tobasef a . frombasef a) . read $ show a

ascend a = iterate (flip frombasef . read $ show a) a

--descending dungeon-like numbers, parenthesized bottom-up
--dDungeon [10..] = OEIS A121295
dDungeon (x:xs) = map fst $ iterate (uncurry convert) (x, xs)
  where convert x (y:ys) = (frombasei y $ tobasei (10 :: Int) x, ys)

--descending dungeon-like numbers, parenthesized top-down
--dDungeonr [10..] = OEIS A121296
dDungeonr xs = [convert $ take n xs | n <- [1..]]
  where convert = foldr1 (\a -> frombasei a . tobasei (10 :: Int))
