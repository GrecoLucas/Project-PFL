import qualified Data.List
import qualified Data.Bits
import qualified Data.Array

type City = Int
type Path = [City]
type Distance = Int
type RoadMap = [(City, City, Distance)]

-- Function to retrieve all cities from the roadmap
cities :: RoadMap -> [City]
cities r = Data.List.nub ([c | (c, _, _) <- r] ++ [c | (_, c, _) <- r])

-- Function to check if two cities are adjacent
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((a,b,_):xs) c1 c2 
    | (c1 == a && c2 == b) || (c1 == b && c2 == a) = True
    | c1 == c2 = True
    | otherwise = areAdjacent xs c1 c2

-- Function to get the distance between two cities
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((a,b,d):xs) c1 c2
    | (c1 == a && c2 == b) || (c1 == b && c2 == a) = Just d
    | otherwise = distance xs c1 c2

-- Function to find all adjacent cities with their distances
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((a,b,d):xs) c1 
    | a == c1 = (b,d) : adjacent xs c1
    | b == c1 = (a,d) : adjacent xs c1
    | otherwise = adjacent xs c1

-- Function to compute the total distance of a path
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [x] = Just 0
pathDistance r (x:y:xs) = case distance r x y of
    Just d -> case pathDistance r (y:xs) of
        Just d' -> Just (d + d')
        Nothing -> Nothing
    Nothing -> Nothing

-- Auxiliary functions to find the city with the most adjacents
numberOfAdj :: RoadMap -> City -> Int
numberOfAdj [] _ = 0
numberOfAdj ((a,b,_):xs) c1
    | a == c1 = 1 + numberOfAdj xs c1
    | b == c1 = 1 + numberOfAdj xs c1
    | otherwise = numberOfAdj xs c1

-- Function to list the cities with the number of citys adjacent to them
adjacentList :: RoadMap -> [(City, Int)]
adjacentList r = [(city, numberOfAdj r city) | city <- cities r]

-- Function to find all the cities with the most adjacents
rome :: RoadMap -> [City]
rome r = [city | (city, adjCount) <- adjacentList r, adjCount == maxAdj]
    where 
        maxAdj = maximum [adjCount | (_, adjCount) <- adjacentList r]

-- Function to find adjacent cities
adjacentCities :: RoadMap -> City -> [City]
adjacentCities [] _ = []
adjacentCities ((a, b, _):xs) c
    | a == c = b : adjacentCities xs c
    | b == c = a : adjacentCities xs c
    | otherwise = adjacentCities xs c

-- Depth-First Search (DFS) to visit all accessible cities from a given city
dfs :: City -> RoadMap -> [City] -> [City]
dfs c r visited
    | c `elem` visited = visited
    | otherwise = foldl (\acc city -> dfs city r acc) (c : visited) (adjacentCities r c)

-- Function to check if the graph is strongly connected
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected r =
    let allCities = cities r
        checkCity c = length (dfs c r []) == length allCities
    in all checkCity allCities

-- Function to find all paths between two cities using DFS
dfsPaths :: RoadMap -> City -> City -> Path -> [(Path, Distance)]
dfsPaths graph current target path
    | current == target = case pathDistance graph (path ++ [current]) of
        Just d -> [(path ++ [current], d)]
        Nothing -> []
    | current `elem` path = []
    | otherwise = concat [dfsPaths graph neighbor target (path ++ [current]) | (neighbor, _) <- adjacent graph current]

-- Function to find the shortest path between two cities
shortestPath :: RoadMap -> City -> City -> (Path, Distance)
shortestPath graph start end = 
    let allPaths = dfsPaths graph start end []
    in if null allPaths then ([], 0)
       else Data.List.minimumBy (\(_, d1) (_, d2) -> compare d1 d2) allPaths

-- Function to build the distance matrix
buildDistanceMatrix :: RoadMap -> Data.Array.Array (Int, Int) Distance
buildDistanceMatrix roadmap = Data.Array.array bounds [((i, j), dist i j) | i <- [0..n-1], j <- [0..n-1]]
  where
    citiesList = cities roadmap
    n = length citiesList
    cityIndex c = case Data.List.elemIndex c citiesList of
        Just idx -> idx
        Nothing -> error "City not found"
    bounds = ((0, 0), (n - 1, n - 1))
    dist i j
        | i == j = 0
        | otherwise = case Data.List.find (\(x, y, _) -> (cityIndex x == i && cityIndex y == j) || (cityIndex x == j && cityIndex y == i)) roadmap of
            Just (_, _, d) -> d
            Nothing -> maxBound `div` 2  -- Represents "infinite"

-- Held-Karp algorithm to solve the TSP
travelSales :: RoadMap -> Path
travelSales roadmap = map indexCity (constructPath finalMask lastCity)
  where
    citiesList = cities roadmap
    n = length citiesList
    cityIndices = [0..n-1]
    cityIndex c = case Data.List.elemIndex c citiesList of
        Just idx -> idx
        Nothing -> error "City not found"
    indexCity i = citiesList !! i
    distMatrix = buildDistanceMatrix roadmap
    finalMask = (1 `Data.Bits.shiftL` n) - 1
    dp :: Data.Array.Array (Int, Int) (Distance, Maybe Int)
    dp = Data.Array.array ((0,0), (finalMask, n-1)) [((mask, u), value mask u) | mask <- [0..finalMask], u <- cityIndices]

    value mask u
        | mask == (1 `Data.Bits.shiftL` u) =
            if u == 0 then (0, Nothing)
            else if distMatrix Data.Array.! (0, u) < maxBound `div` 2 then (distMatrix Data.Array.! (0, u), Just 0)
            else (maxBound `div` 2, Nothing)
        | (mask Data.Bits..&. (1 `Data.Bits.shiftL` u)) == 0 = (maxBound `div` 2, Nothing)
        | otherwise =
            let prevMask = clearBit mask u
                candidates = [ (fst (dp Data.Array.! (prevMask, k)) + distMatrix Data.Array.! (k, u), Just k)
                             | k <- cityIndices
                             , (prevMask Data.Bits..&. (1 `Data.Bits.shiftL` k)) /= 0
                             , distMatrix Data.Array.! (k, u) < maxBound `div` 2
                             ]
            in if null candidates
               then (maxBound `div` 2, Nothing)
               else minimumBy fst candidates

    -- Find the city with the minimal cost to return to the starting city
    (minCost, Just lastCity) = minimum [ (fst (dp Data.Array.! (finalMask, u)) + distMatrix Data.Array.! (u, 0), Just u)
                                       | u <- cityIndices
                                       , u /= 0
                                       , fst (dp Data.Array.! (finalMask, u)) < maxBound `div` 2
                                       , distMatrix Data.Array.! (u, 0) < maxBound `div` 2
                                       ]

    -- Function to reconstruct the minimum path
    constructPath mask u
        | mask == (1 `Data.Bits.shiftL` u) = [u]
        | otherwise = case snd (dp Data.Array.! (mask, u)) of
            Just k  -> constructPath (clearBit mask u) k ++ [u]
            Nothing -> error "Path not found"

    -- Helper function to get the minimum based on a projection
    minimumBy :: Ord b => (a -> b) -> [a] -> a
    minimumBy _ [] = error "minimumBy: empty list"
    minimumBy f xs = foldl1 (\x y -> if f x <= f y then x else y) xs

    -- Function to clear the bit at position 'u'
    clearBit mask u = mask Data.Bits..&. Data.Bits.complement (1 `Data.Bits.shiftL` u)

-- Function for brute-force TSP (undefined for groups of 2)
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined

-- Example graphs to test your work
gTest1 :: RoadMap
gTest1 = [(7,6,1),(8,2,2),(6,5,2),(0,1,4),(2,5,4),(8,6,6),(2,3,7),(7,8,7),(0,7,8),
          (1,2,8),(3,4,9),(5,4,10),(1,7,11),(3,5,14),(7,5,14),(8,3,15),(6,1,10),
          (0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest2 :: RoadMap
gTest2 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest3 :: RoadMap -- Unconnected graph
gTest3 = [(0,1,4),(2,3,2)]

gTest4 :: RoadMap
gTest4 = [(0,1,4),(0,2,1),(2,1,1),(1,2,10),(1,3,2),(2,3,3)]

gTest5 :: RoadMap
gTest5 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(1,4,30),
          (2,3,30),(2,4,20),(2,5,25),(3,4,15),(3,5,20),(4,5,30),
          (1,6,10),(6,7,10),(5,7,20),(4,6,25),(0,5,15),(7,8,5),
          (5,8,10),(2,7,15),(1,8,20)]