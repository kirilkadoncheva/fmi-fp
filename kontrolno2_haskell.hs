summList :: [(a,b,Double)] -> Double 
summList [] = 0.0
summList ((x,y,z) : rest) = z + (summList rest)

len :: [t] -> Double
len [] = 0.0
len (x:xs) = 1 + len xs

getAveragePrice :: [([Char], [Char], Double)] -> ([Char], [Char]) ->  Double
getAveragePrice xs (xshop,xcat) =  (summList listNeeded)/countItems 
                                    where listNeeded = filter (\(shop,category,price) -> shop == xshop && category == xcat) xs
                                          countItems = len listNeeded

categorised :: [([Char], [Char], Double)] ->  [([Char], [Char], Double)]
categorised [] = []
categorised ((a,b,c) : rest) = [(a,b, averagePrice) ] ++ categorised filteredRest
                                 where averagePrice = getAveragePrice ((a,b,c) : rest) (a,b){-} ++ (categorised filteredRest)
                                       filteredRest = filter (\(x,y,z) -> not (x==a && y==b)) rest-}
                                       filteredRest = filter (\(x,y,z) -> not (x==a && y==b)) rest



items =[("lidl","veg",1.5), ("kauf","k",23),("lidl","veg",3), ("kauf","k",27),("fant","k",6),("fant","l",6)]