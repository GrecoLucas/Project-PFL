import Data.List
import Data.Bits
import Data.Ord (comparing)

--dynamic programming e bit masking, travelers salesman se o numero em binario só for tudo a 1, mto eficiente


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = Int
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- nub remove duplicates from a list
-- add all the first elements of the roadmap and all the second elements of the roadmap to a list and remove duplicates
cities :: RoadMap -> [City]
cities r = nub ([c | (c, _, _) <- r] ++ [c | (_, c, _) <- r])


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


-- LISTA ADJACENTE MELHOR ALGORÍTIMO
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined


travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work

gTest1 :: RoadMap
gTest1 = [(7,6,1),(8,2,2),(6,5,2),(0,1,4),(2,5,4),(8,6,6),(2,3,7),(7,8,7),(0,7,8),(1,2,8),(3,4,9),(5,4,10),(1,7,11),(3,5,14),(7,5,14),(8,3,15)]

gTest2 :: RoadMap
gTest2 = [(0,1,10),(0,2,15),(0,3,20),(1,2,35),(1,3,25),(2,3,30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [(0,1,4),(2,3,2)]


gTest4 :: RoadMap
gTest4 = [(0,1,4),(1,2,3),(2,3,2),(3,0,1),(0,2,5),(1,3,4)]
