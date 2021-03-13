groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f xs = [head x : |  tail x | x <- abc]
 where xfxList = map (id &&& f) xs
       grouped = groupBy (\(_, fx) (_, fy) -> fx == fy) xfxList
       abc = [map fst x | x <- grouped]