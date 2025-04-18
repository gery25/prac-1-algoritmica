-- Translation of illa.py to Haskell
import System.IO
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (elemIndex, sortBy)
import Data.Ord (comparing)
import System.Directory (doesFileExist)

-- Types

-- Graph represented as a Map from a node to its parent
type Graph = Map.Map String String

type Boys = [String]
type Jelouses = Map.Map String String
type Friends = Map.Map String String

-- File reading
illa :: FilePath -> IO (Boys, Jelouses, Friends)
illa filepath = do
    -- Check if the file exists
    exists <- doesFileExist filepath
    if not exists
        then error $ "File not found: " ++ filepath
        else do
            -- Read the file and parse its content
            content <- readFile filepath
            let linesContent = lines content
                triples = map words linesContent
                names = map (!! 0) triples
                jelouses = Map.fromList [(x!!0, x!!1) | x <- triples]
                friends  = Map.fromList [(x!!0, x!!2) | x <- triples]
            return (names, jelouses, friends)

-- DFS (Depth-First Search)

-- Build a child graph from a parent graph
buildChildGraph :: Graph -> Map.Map String [String]
buildChildGraph g = Map.fromListWith (++) [(v, [k]) | (k, v) <- Map.toList g]

-- Perform DFS on the graph
dfs :: Graph -> String -> [String] -> [String]
dfs g root boys = go Set.empty [root] []
  where
    -- Map of children for each node
    childrenMap = buildChildGraph g
    -- Priority function for sorting children
    priority x = maybe (length boys) id (elemIndex x boys)
    -- Recursive DFS traversal
    go _ [] path = path
    go visited (n:stack) path
      | Set.member n visited = go visited stack path -- Skip visited nodes
      | otherwise =
          let visited' = Set.insert n visited
              children = Map.findWithDefault [] n childrenMap
              sorted = reverse $ sortBy (comparing priority) children
          in go visited' (sorted ++ stack) (path ++ [n])

-- Create a graph excluding the root
createGraph :: Jelouses -> String -> Graph
createGraph jelouses root = Map.delete root jelouses

-- Follow the path from a node to its parent
path :: Graph -> String -> [String]
path graph node = case Map.lookup node graph of
    Nothing -> [node]
    Just next -> node : path graph next

-- Create a priority list for traversal
createPriority :: String -> [String] -> String -> [String] -> [String]
createPriority root unprior friend boys =
    let base = [root] ++ [friend | friend /= root]
    in base ++ [x | x <- boys, x `notElem` base, x `notElem` unprior]

-- Verify if the solution is correct
isCorrect :: [String] -> Jelouses -> Friends -> Bool
isCorrect sol jelouses friends = all check sol
  where
    -- Map positions of nodes in the solution
    pos = Map.fromList (zip sol [0..])
    check node = case (Map.lookup node jelouses, Map.lookup node friends) of
        (Just j, Just f) ->
            case (Map.lookup node pos, Map.lookup j pos, Map.lookup f pos) of
                (Just p, Just pj, Just pf) ->
                    p < pj && (pf > pj || (pf > p && pf < pj)) -- Check jealousy and friendship constraints
                _ -> True
        _ -> True

-- Detect cycles in the graph
foundCicle :: Graph -> Bool -> [String]
foundCicle graph needRoots = go (Map.keys graph) Set.empty []
  where
    go [] _ roots = if needRoots then roots else []
    go (n:ns) visited roots
        | n `Set.member` visited = go ns visited roots -- Skip visited nodes
        | otherwise =
            let (cycle, visited', r) = explore n Set.empty visited []
            in if cycle && not needRoots then [] else go ns visited' (roots ++ r)

    explore current path visited acc = case Map.lookup current graph of
        Nothing -> (False, Set.insert current visited, acc ++ [current])
        Just next
            | current `Set.member` path -> (True, visited, acc) -- Cycle detected
            | current `Set.member` visited -> (False, visited, acc) -- Already visited
            | otherwise -> explore next (Set.insert current path) (Set.insert current visited) (acc ++ [current])

-- Break cycles in the graph
breakCicles :: Graph -> String -> Friends -> (Graph, Bool)
breakCicles graph node friends = go (Map.insert (friends Map.! node) node graph) Set.empty node
  where
    go g seen current
        | foundCicle g False == [] = (g, True) -- No cycles found
        | show g `Set.member` seen = (g, False) -- Cycle cannot be resolved
        | otherwise =
            let n = friends Map.! current
                g1 = Map.delete n g
                n2 = friends Map.! (friends Map.! n)
                g2 = Map.insert n2 (friends Map.! n) g1
            in go g2 (Set.insert (show g) seen) n

-- Build the graph and determine if it's solvable
buildGraph :: [String] -> Jelouses -> Friends -> String -> (Bool, Graph, [String])
buildGraph boys jelouses friends root =
    let graph = createGraph jelouses root
        jr = jelouses Map.! root
        fr = friends Map.! root
        (pr, up) = (path graph fr, path graph jr)
        priority = createPriority root up fr boys
        (g, ok) = if jr `elem` pr then breakCicles graph root friends else (graph, True)
    in (ok, g, priority)

-- Print the solution
printSolution :: [String] -> IO ()
printSolution sol = putStrLn $ unwords sol

-- Evaluate the solution
evaluate :: [String] -> Jelouses -> Friends -> IO ()
evaluate boys jelouses friends =
    let roots = foundCicle jelouses True
    in try roots
  where
    try [] = putStrLn "impossible" -- No valid roots
    try (r:rs) =
        let (ok, graph, prio) = buildGraph boys jelouses friends r
        in if not ok then try rs else
            let sol = dfs graph r prio
            in if isCorrect sol jelouses friends then printSolution sol else try rs

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        (file:_) -> do
            (boys, jelouses, friends) <- illa file
            evaluate boys jelouses friends
        _ -> putStrLn "Usage: ./program <input_file>"
