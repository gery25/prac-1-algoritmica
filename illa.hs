import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, delete, intercalate, sortBy)
import qualified Data.Set as Set

-- Tipus

type Name = String
type Jelouses = Map.Map Name Name
type Friends = Map.Map Name Name
type Graph = Map.Map Name Name

type Adjacency = Map.Map Name [Name]

-- Llegeix fitxer i construeix dades
parseFile :: FilePath -> IO ([Name], Jelouses, Friends)
parseFile filename = do
    contents <- readFile filename
    let linesData = lines contents
        entries = map words linesData
        boys = map (!! 0) entries
        jelouses = Map.fromList [(x, y) | [x, y, _] <- entries]
        friends  = Map.fromList [(x, z) | [x, _, z] <- entries]
    return (boys, jelouses, friends)

-- Cicle?
hasCycle :: Graph -> Bool
hasCycle graph = any (detectCycle Set.empty) (Map.keys graph)
  where
    detectCycle visited node
      | Set.member node visited = True
      | otherwise = case Map.lookup node graph of
          Nothing -> False
          Just next -> detectCycle (Set.insert node visited) next

-- Camí des d'un node
pathFrom :: Graph -> Name -> [Name]
pathFrom graph start = go start []
  where
    go node acc = case Map.lookup node graph of
        Nothing -> reverse acc
        Just next -> go next (node:acc)

-- Crear graf excloent arrel
createGraph :: Jelouses -> Name -> Graph
createGraph jelouses root = Map.delete root jelouses

-- Crear llista de prioritats
createPriority :: Name -> [Name] -> Name -> [Name] -> [Name]
createPriority root unprioritizedPath friendRoot boys =
    let firsts = [root, friendRoot]
        rest = filter (`notElem` (firsts ++ unprioritizedPath)) boys
    in firsts ++ rest

-- Crear camins
createPaths :: Graph -> Name -> Name -> ([Name], [Name])
createPaths graph jealous friend = (pathFrom graph friend, pathFrom graph jealous)

-- Construir graf i comprovar validesa
buildGraph :: [Name] -> Jelouses -> Friends -> Name -> (Bool, Graph, [Name])
buildGraph boys jelouses friends root =
    let graph = createGraph jelouses root
        jealousRoot = fromMaybe "" $ Map.lookup root jelouses
        friendRoot = fromMaybe "" $ Map.lookup root friends
        (pathFriend, unprioritized) = createPaths graph jealousRoot friendRoot
        priority = createPriority root unprioritized friendRoot boys
        cycleInPath = jealousRoot `elem` pathFriend
        (graph', possible) = if cycleInPath
                             then breakCycles graph root friends
                             else (graph, True)
    in (possible, graph', priority)

-- Trencar cicles
breakCycles :: Graph -> Name -> Friends -> (Graph, Bool)
breakCycles graph node friends = go graph node Set.empty
  where
    go g n seen
      | hasCycle g =
          case Map.lookup n friends of
            Nothing -> (g, False)
            Just f ->
              let g' = Map.insert f n (Map.delete n g)
              in if Set.member (show g') seen
                 then (g', False)
                 else go g' f (Set.insert (show g') seen)
      | otherwise = (g, True)

-- DFS
buildAdjacency :: Graph -> Adjacency
buildAdjacency g = Map.fromListWith (++) [(father, [child]) | (child, father) <- Map.toList g]

dfs :: Adjacency -> [Name] -> Name -> [Name]
dfs graph priority root = go Set.empty [root]
  where
    prioIndex n = fromMaybe (length priority) (elemIndex n priority)
    go _ [] = []
    go visited (n:ns)
      | Set.member n visited = go visited ns
      | otherwise =
          let children = Map.findWithDefault [] n graph
              sorted = reverse $ sortBy (\a b -> compare (prioIndex a) (prioIndex b)) children
          in n : go (Set.insert n visited) (sorted ++ ns)

-- Comprovació de solució
isCorrect :: [Name] -> Jelouses -> Friends -> Bool
isCorrect solution jelouses friends =
    all check solution
  where
    pos = Map.fromList (zip solution [0..])
    check node =
      case (Map.lookup node jelouses, Map.lookup node friends) of
        (Just jeal, Just friend) ->
            case (Map.lookup node pos, Map.lookup jeal pos, Map.lookup friend pos) of
              (Just p1, Just p2, Just pf) ->
                  not (p1 < p2 && not (p1 < pf && pf < p2))
              _ -> True
        _ -> True

-- Avaluar possibles arrels i buscar solució
evaluate :: [Name] -> Jelouses -> Friends -> IO ()
evaluate boys jelouses friends =
    let roots = Map.keys jelouses
    in tryRoots roots
  where
    tryRoots [] = putStrLn "impossible"
    tryRoots (r:rs) =
        let (possible, graph, priority) = buildGraph boys jelouses friends r
            adj = buildAdjacency graph
            solution = dfs adj priority r
        in if not possible || not (isCorrect solution jelouses friends)
            then tryRoots rs
            else putStrLn $ unwords solution

-- Main
main :: IO ()
main = do
    args <- getArgs
    case args of
      [filename] -> do
          (boys, jelouses, friends) <- parseFile filename
          evaluate boys jelouses friends
      _ -> putStrLn "Ús: ./programa fitxer.txt"
