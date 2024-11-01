import qualified Data.List
import qualified Data.Bits
import qualified Data.Array
import Data.Maybe (fromJust, isNothing)


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.
type City = Int
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]

-- nub remove duplicates from a list
-- add all the first elements of the roadmap and all the second elements of the roadmap to a list and remove duplicates with nub
cities :: RoadMap -> [City]
cities r = Data.List.nub ([c | (c, _, _) <- r] ++ [c | (_, c, _) <- r])

-- if the first element its equal to one of the input cities, and the second element its equal to the other input city, return true
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((a,b,_):xs) c1 c2 
    | (c1 == a && c2 == b) || (c1 == b && c2 == a) = True
    | c1 == c2 = True
    | otherwise = areAdjacent xs c1 c2

-- if the first element its equal to one of the input cities, and the second element its equal to the other input city, return the distance
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((a,b,d):xs) c1 c2
    | (c1 == a && c2 == b) || (c1 == b && c2 == a) =  Just d
    | otherwise = distance xs c1 c2

-- interate over the roadmap and if the first or second element its equal to the input city, add the other element to the list with the distance
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((a,b,d):xs) c1 
    | a == c1 = (b,d) : adjacent xs c1
    | b == c1 = (a,d) : adjacent xs c1
    | otherwise = adjacent xs c1

-- interate over the roadmap, cheking if the distance between the first and second element of the path exists, if it does, add it to the d variable 
-- if the distance dooesnt exist, return nothing
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing
pathDistance _ [] = Just 0
pathDistance _ [x] = Just 0
pathDistance r (x:y:xs) = case distance r x y of
    Just d -> case pathDistance r (y:xs) of
        Just d' -> Just (d + d')
        Nothing -> Nothing
    Nothing -> Nothing
    

--aux
-- return the number of cities that are adjacents of a city
numberoofadj :: RoadMap -> City -> Int
numberoofadj [] _ = 0
numberoofadj ((a,b,_):xs) c1
    | a == c1 = 1 + numberoofadj xs c1
    | b == c1 = 1 + numberoofadj xs c1
    | otherwise = numberoofadj xs c1

-- aux
-- return a list with the cities and the number of adjacents of each city
adjacentlist :: RoadMap -> [(City,Int)]
adjacentlist r = [(city, numberoofadj r city) | city <- cities r]

-- return the city with the most adjacents
-- get the maximum number of adjacents and return all the city with that number of adjacents
rome :: RoadMap -> [City]
rome r = [city | (city, adjcount) <- adjacentlist r, adjcount == max]
    where 
        max = maximum [adjcount | (_, adjcount) <- adjacentlist r]
 


-- Função auxiliar para encontrar as cidades adjacentes de uma cidade
adjacentCities :: RoadMap -> City -> [City]
adjacentCities [] _ = []
adjacentCities ((a, b, _):xs) c
    | a == c = b : adjacentCities xs c
    | b == c = a : adjacentCities xs c
    | otherwise = adjacentCities xs c

-- Função DFS (Depth First Search) para visitar todas as cidades acessíveis a partir de uma cidade
dfs :: City -> RoadMap -> [City] -> [City]
dfs c r visited
    | c `elem` visited = visited  -- Se a cidade já foi visitada, retorna a lista de cidades visitadas
    | otherwise = foldl (\acc city -> dfs city r acc) (c : visited) (adjacentCities r c)
    -- Explora recursivamente as cidades adjacentes e acumula as visitadas

-- Função que verifica se o grafo é fortemente conectado
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected r =
    let allCities = cities r  -- Obtém todas as cidades do RoadMap
        -- Função auxiliar que verifica se todas as cidades são alcançáveis a partir de uma cidade c
        checkCity c = length (dfs c r []) == length allCities
    in all checkCity allCities  -- Verifica se todas as cidades podem ser ponto inicial de uma DFS válida


dfs2 :: RoadMap -> City -> City -> Path -> [(Path, Distance)]
dfs2 graph current target path
    | current == target = case pathDistance graph (path ++ [current]) of
        Just d -> [(path ++ [current], d)]
        Nothing -> []
    | current `elem` path = []  -- Evita ciclos
    | otherwise = concat [dfs2 graph neighbor target (path ++ [current]) | (neighbor, _) <- adjacent graph current]


shortestPath :: RoadMap -> City -> City -> (Path, Distance)
shortestPath graph start end = 
    let allPaths = dfs2 graph start end []  -- Obtém todos os caminhos
    in if null allPaths then ([], 0)  -- Se não houver caminhos, retorna ([], 0)
       else let minPath = Data.List.minimumBy (\(_, d1) (_, d2) -> compare d1 d2) allPaths -- Encontra o caminho com a menor distância
            in minPath  -- Retorna o menor caminho




-- Função para construir a matriz de distâncias
buildDistanceMatrix :: RoadMap -> Data.Array.Array (Int, Int) Distance
buildDistanceMatrix roadmap = Data.Array.array bounds [((i, j), dist i j) | i <- [0..n-1], j <- [0..n-1]]
  where
    citiesList = Data.List.nub [c | (c1, c2, _) <- roadmap, c <- [c1, c2]]
    n = length citiesList
    cityIndex c = fromJust $ Data.List.elemIndex c citiesList
    bounds = ((0, 0), (n-1, n-1))
    dist i j
      | i == j = 0
      | otherwise = case Data.List.find (\(x, y, _) -> (cityIndex x == i && cityIndex y == j) || (cityIndex x == j && cityIndex y == i)) roadmap of
          Just (_, _, d) -> d
          Nothing -> maxBound `div` 2  -- Representa "infinito"

-- Algoritmo Held-Karp para resolver o TSP
travelSales :: RoadMap -> Path
travelSales roadmap = map indexCity (constructPath finalMask lastCity)
  where
    citiesList = Data.List.nub [c | (c1, c2, _) <- roadmap, c <- [c1, c2]]
    n = length citiesList
    cityIndices = [0..n-1]
    cityIndex c = fromJust $ Data.List.elemIndex c citiesList
    indexCity i = citiesList !! i
    distMatrix = buildDistanceMatrix roadmap
    finalMask = (1 `Data.Bits.shiftL` n) - 1

    -- Tabela dp[mask][u] = (distância mínima para alcançar 'mask' terminando em 'u', cidade anterior)
    dp :: Data.Array.Array (Int, Int) (Distance, Maybe Int)
    dp = Data.Array.array ((0,0), (finalMask,n-1)) [((mask,u), value mask u) | mask <- [0..finalMask], u <- cityIndices]

    value mask u
      | mask == (1 `Data.Bits.shiftL` u) =
          if u == 0 then (0, Nothing)
          else if distMatrix Data.Array.! (0, u) < maxBound `div` 2 then (distMatrix Data.Array.! (0, u), Just 0)
          else (maxBound `div` 2, Nothing)
      | (mask Data.Bits..&. (1 `Data.Bits.shiftL` u)) == 0 = (maxBound `div` 2, Nothing)
      | otherwise =
          let prevMask = mask `clearBit` u
              candidates = [ (fst (dp Data.Array.! (prevMask,k)) + distMatrix Data.Array.! (k,u), Just k)
                           | k <- cityIndices
                           , (prevMask Data.Bits..&. (1 `Data.Bits.shiftL` k)) /= 0
                           , distMatrix Data.Array.! (k,u) < maxBound `div` 2
                           ]
          in if null candidates
             then (maxBound `div` 2, Nothing)
             else minimumBy fst candidates

    -- Encontra a cidade final com menor custo para voltar à cidade inicial
    (minCost, Just lastCity) = minimum [ (fst (dp Data.Array.! (finalMask, u)) + distMatrix Data.Array.! (u, 0), Just u)
                                       | u <- cityIndices
                                       , u /= 0
                                       , fst (dp Data.Array.! (finalMask, u)) < maxBound `div` 2
                                       , distMatrix Data.Array.! (u, 0) < maxBound `div` 2
                                       ]

    -- Função para reconstruir o caminho mínimo
    constructPath mask u
      | mask == (1 `Data.Bits.shiftL` u) = [u]
      | otherwise = case snd (dp Data.Array.! (mask, u)) of
          Just k  -> constructPath (mask `clearBit` u) k ++ [u]
          Nothing -> error "Caminho não encontrado"

    -- Função auxiliar para obter o mínimo baseado em uma projeção
    minimumBy :: Ord b => (a -> b) -> [a] -> a
    minimumBy _ [] = error "minimumBy: empty list"
    minimumBy f xs = foldl1 (\x y -> if f x <= f y then x else y) xs

    -- Função para limpar o bit na posição 'u'
    clearBit mask u = mask Data.Bits..&. Data.Bits.complement (1 `Data.Bits.shiftL` u)



tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work

gTest1 :: RoadMap
gTest1 = [(7,6,1),(8,2,2),(6,5,2),(0,1,4),(2,5,4),(8,6,6),(2,3,7),(7,8,7),(0,7,8),(1,2,8),(3,4,9),(5,4,10),(1,7,11),(3,5,14),(7,5,14),(8,3,15), (6,1,10), (0,2,15), (0,3,20), (1,2,35), (1,3,25), (2,3,30)]

gTest2 :: RoadMap
gTest2 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [(0,1,4),(2,3,2)]

gTest4 :: RoadMap
gTest4 = [(0,1,4),(0,2,1),(2,1,1),(1,2,10),(1,3,2),(2,3,3)]

gTest5 :: RoadMap
gTest5 = [
    (0, 1, 10), (0, 2, 15), (0, 3, 20),
    (1, 2, 35), (1, 3, 25), (1, 4, 30),
    (2, 3, 30), (2, 4, 20), (2, 5, 25),
    (3, 4, 15), (3, 5, 20), (4, 5, 30),
    (1, 6, 10), (6, 7, 10), (5, 7, 20),
    (4, 6, 25), (0, 5, 15), (7, 8, 5),
    (5, 8, 10), (2, 7, 15), (1, 8, 20)]