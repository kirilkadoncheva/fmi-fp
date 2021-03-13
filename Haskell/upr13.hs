data Meal = Meal{title :: String, price :: Double} deriving Show

type Order = [Meal]

data CLient = Client {name :: String, orders :: [Order]} deriving Show

maximumBy :: Ord b => (a -> b) ->[a] -> a
maximumBy f = foldl1 (maxBy f)
       where maxBy f x y
               | f y > f x = y
               |otherwise  = x

mostExpensiveClientMeal :: Map String Client -> String -> Maybe (Client, Meal)
mostExpensiveClientMeal clients clientName