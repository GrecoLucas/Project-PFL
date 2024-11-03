import qualified Data.List
import qualified Data.Bits
import qualified Data.Array

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City, City, Distance)]

-- cities -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Extracts a list of unique cities from the RoadMap by collecting all starting and ending cities and removing duplicates.
cities :: RoadMap -> [City]
cities r = Data.List.nub ([c | (c, _, _) <- r] ++ [c | (_, c, _) <- r])

-- areAdjacent --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Checks if two cities are directly connected in the RoadMap.
-- Returns True if a direct road exists between City1 and City2, or if they are the same city, otherwise returns False.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((a, b, _):xs) c1 c2 
    | (c1 == a && c2 == b) || (c1 == b && c2 == a) = True
    | c1 == c2 = True
    | otherwise = areAdjacent xs c1 c2

-- distance -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Finds the distance between two adjacent cities in the RoadMap.
-- If the cities are connected by a direct road, returns `Just Distance`, otherwise returns `Nothing`.
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((a, b, d):xs) c1 c2
    | (c1 == a && c2 == b) || (c1 == b && c2 == a) = Just d
    | otherwise = distance xs c1 c2

-- adjacent -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Lists all cities adjacent to a given city in the RoadMap, including the distance to each.
-- Returns a list of tuples (AdjacentCity, Distance).
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((a, b, d):xs) c1 
    | a == c1 = (b, d) : adjacent xs c1
    | b == c1 = (a, d) : adjacent xs c1
    | otherwise = adjacent xs c1

-- pathDistance -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Computes the total distance for a given path of cities in the RoadMap, if the path is valid (i.e., all cities are adjacent).
-- Returns `Just TotalDistance` for a valid path, or `Nothing` if any two consecutive cities are not connected.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [x] = Just 0
pathDistance r (x:y:xs) = case distance r x y of
    Just d -> case pathDistance r (y:xs) of
        Just d' -> Just (d + d')
        Nothing -> Nothing
    Nothing -> Nothing

-- rome -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Auxiliary function to count the number of adjacent cities for a specific city in the RoadMap.
numberOfAdj :: RoadMap -> City -> Int
numberOfAdj [] _ = 0
numberOfAdj ((a, b, _):xs) c1
    | a == c1 = 1 + numberOfAdj xs c1
    | b == c1 = 1 + numberOfAdj xs c1
    | otherwise = numberOfAdj xs c1

-- Creates a list of cities with their respective counts of adjacent cities.
adjacentList :: RoadMap -> [(City, Int)]
adjacentList r = [(city, numberOfAdj r city) | city <- cities r]

-- Finds cities with the maximum number of direct adjacencies in the RoadMap.
-- Returns a list of cities that have the highest adjacency count.
rome :: RoadMap -> [City]
rome r = [city | (city, adjCount) <- adjacentList r, adjCount == maxAdj]
    where 
        maxAdj = maximum [adjCount | (_, adjCount) <- adjacentList r]

-- strongly Connected------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Lists all cities directly connected to a given city in the RoadMap.
adjacentCities :: RoadMap -> City -> [City]
adjacentCities [] _ = []
adjacentCities ((a, b, _):xs) c
    | a == c = b : adjacentCities xs c
    | b == c = a : adjacentCities xs c
    | otherwise = adjacentCities xs c

-- Performs Depth-First Search (DFS) from a given city to explore all accessible cities in the RoadMap.
dfs :: City -> RoadMap -> [City] -> [City]
dfs c r visited
    | c `elem` visited = visited
    | otherwise = foldl (\acc city -> dfs city r acc) (c : visited) (adjacentCities r c)

-- Checks if all cities in the RoadMap are strongly connected.
-- A RoadMap is strongly connected if every city is reachable from every other city.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected r =
    let allCities = cities r
        checkCity c = length (dfs c r []) == length allCities
    in all checkCity allCities

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- shortest Path-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Auxiliary function to find all paths between two cities using DFS
dfsPaths :: RoadMap -> City -> City -> Path -> [(Path, Distance)]
dfsPaths graph current target path
    | current == target = case pathDistance graph (path ++ [current]) of -- if the current city is the target, calculate the distance
        Just d -> [(path ++ [current], d)] -- if the distance is valid, return the path and the distance
        Nothing -> []
    | current `elem` path = [] -- if the current city is already in the path, return an empty list
    | otherwise = concat [dfsPaths graph neighbor target (path ++ [current]) | (neighbor, _) <- adjacent graph current] -- otherwise, recursively find the paths

-- Function to find the shortest path between two cities
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath graph start end 
    | start == end = [[start]] -- if the start and end cities are the same, return the city as a single-element path
    | otherwise = 
        let allPaths = dfsPaths graph start end [] -- find all paths between the start and end cities
        in if null allPaths then []
        else [fst (Data.List.minimumBy (\(_, d1) (_, d2) -> compare d1 d2) allPaths)] -- return the path with the minimum distance

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Traveling Salesman Problem-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Auxiliary function to build the distance matrix
buildDistanceMatrix :: RoadMap -> Data.Array.Array (Int, Int) Distance
buildDistanceMatrix roadmap = Data.Array.array bounds [((i, j), dist i j) | i <- [0..n], j <- [0..n]]
  where
    citiesList = cities roadmap        -- List of all cities in the roadmap
    n = length citiesList              -- Total number of cities
    bounds = ((0, 0), (n, n))          -- Bounds for the distance matrix array
    dist i j
        | i == j = 0                   -- Distance from a city to itself is 0
        | otherwise = case distance roadmap (citiesList !! i) (citiesList !! j) of
            Just d  -> d               -- Direct distance between city i and city j
            Nothing -> maxBound `div` 2  -- If no direct path, represent distance as "infinite"


-- Held-Karp algorithm to solve the Traveling Salesman Problem (TSP)
travelSales :: RoadMap -> Path
travelSales roadmap
    | not (isStronglyConnected roadmap) = []  -- If the graph is not strongly connected, return an empty list
    | otherwise = 
        let path = map indexCity (constructPath finalMask lastCity)  -- Reconstruct the optimal path
        in path ++ [head path]  -- Add the starting city to the end to form a cycle
  where
    -- Prepare initial variables
    citiesList = cities roadmap        -- List of all cities
    n = length citiesList              -- Total number of cities
    cityIndices = [0..n-1]             -- Indices representing each city
    indexCity i = citiesList !! i      -- Retrieve city by index
    distMatrix = buildDistanceMatrix roadmap  -- Generate the distance matrix
    finalMask = (1 `Data.Bits.shiftL` n) - 1  -- Bitmask representing all cities visited

    -- Dynamic programming table to store the minimal distances and previous city
    dp :: Data.Array.Array (Int, Int) (Distance, Maybe Int)
    dp = Data.Array.array ((0,0), (finalMask, n-1)) 
        [ ((mask, u), value mask u)
        | mask <- [0..finalMask], u <- cityIndices ]

    -- Function to compute the minimal cost to reach city u with a set of visited cities represented by mask
    value mask u
        | mask == (1 `Data.Bits.shiftL` u) =
            if u == 0
                then (0, Nothing)  -- Cost to reach the starting city is zero
                else if distMatrix Data.Array.! (0, u) < maxBound `div` 2
                    then (distMatrix Data.Array.! (0, u), Just 0)  -- Direct distance from start to city u
                    else (maxBound `div` 2, Nothing)  -- No direct path from start to city u
        | (mask Data.Bits..&. (1 `Data.Bits.shiftL` u)) == 0 = (maxBound `div` 2, Nothing)  -- City u not in mask
        | otherwise =
            let prevMask = clearBit mask u  -- Remove city u from mask to consider previous cities
                candidates = 
                    [ (fst (dp Data.Array.! (prevMask, k)) + distMatrix Data.Array.! (k, u), Just k)
                    | k <- cityIndices
                    , (prevMask Data.Bits..&. (1 `Data.Bits.shiftL` k)) /= 0  -- City k is in prevMask
                    , distMatrix Data.Array.! (k, u) < maxBound `div` 2       -- There is a path from k to u
                    ]
            in if null candidates
                then (maxBound `div` 2, Nothing)  -- No possible paths to u
                else minimumBy fst candidates      -- Choose the path with minimal cost

    -- Find the last city in the optimal path and the minimal total cost
    (minCost, Just lastCity) =
        minimum [ (fst (dp Data.Array.! (finalMask, u)) + distMatrix Data.Array.! (u, 0), Just u)
        | u <- cityIndices, u /= 0
        , fst (dp Data.Array.! (finalMask, u)) < maxBound `div` 2
        , distMatrix Data.Array.! (u, 0) < maxBound `div` 2 ]

    -- Function to reconstruct the optimal path using the dp table
    constructPath mask u
        | mask == (1 `Data.Bits.shiftL` u) = [u]  -- Base case: only city u is visited
        | otherwise = 
            case snd (dp Data.Array.! (mask, u)) of
                Just k  -> constructPath (clearBit mask u) k ++ [u]  -- Append current city u to path
                Nothing -> error "Path not found"

    -- Helper function to select the minimum element based on a projection function
    minimumBy :: Ord b => (a -> b) -> [a] -> a
    minimumBy _ [] = error "minimumBy: empty list"
    minimumBy f xs = foldl1 (\x y -> if f x <= f y then x else y) xs

    -- Function to clear the bit at position 'u' in the mask
    clearBit mask u =
        mask Data.Bits..&. Data.Bits.complement (1 `Data.Bits.shiftL` u)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Brute force tsp--------------------------------------------------------------------------------------------------------------------------------------------------------------
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Example graphs to test your work -------------------------------------------------------------------------------------------------------------------------------------------------
-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
