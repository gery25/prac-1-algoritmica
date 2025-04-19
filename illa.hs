-- Importació de mòduls necessaris
import System.IO                       -- Per operacions d'entrada/sortida
import System.Environment (getArgs)    -- Per obtenir arguments de línia de comandes
import qualified Data.Map as Map      -- Per utilitzar Maps (diccionaris)
import qualified Data.Set as Set      -- Per utilitzar Sets (conjunts)
import Data.List (elemIndex, sortBy)  -- Per funcions de llistes
import Data.Ord (comparing)           -- Per comparacions
import System.Directory (doesFileExist) -- Per comprovar existència de fitxers

-- Definició de tipus
type Graph = Map.Map String String    -- Graf representat com un Map de node a pare
type Jelouses = Map.Map String String -- Map per emmagatzemar gelosies
type Friends = Map.Map String String  -- Map per emmagatzemar amistats

-- Definició d'un nou tipus per llistes amb functor
newtype ListF a = ListF { getList :: [a] }
  deriving (Show, Eq)

-- Implementació del Functor per ListF
instance Functor ListF where
    fmap f (ListF xs) = ListF (map f xs)  -- Aplica una funció a tots els elements

-- Funció principal de lectura de fitxer
illa :: FilePath -> IO (ListF String, Jelouses, Friends)
illa filepath = do
    exists <- doesFileExist filepath       -- Comprova si existeix el fitxer
    if not exists
        then error $ "File not found: " ++ filepath
        else do
            content <- readFile filepath    -- Llegeix el contingut
            let linesContent = lines content    -- Separa per línies
                triples = map words linesContent -- Separa cada línia en paraules
                names = ListF $ map (!! 0) triples  -- Agafa els noms (primera paraula)
                jelouses = Map.fromList [(x!!0, x!!1) | x <- triples]  -- Crea map de gelosies
                friends  = Map.fromList [(x!!0, x!!2) | x <- triples]  -- Crea map d'amistats
            return (names, jelouses, friends)

-- Construcció del graf de fills
buildChildGraph :: Graph -> Map.Map String [String]
buildChildGraph g = Map.fromListWith (++) [(v, [k]) | (k, v) <- Map.toList g]

-- Implementació de DFS (Cerca en Profunditat)
dfs :: Graph -> String -> ListF String -> ListF String
dfs g root boys = ListF $ go Set.empty [root] []
  where
    childrenMap = buildChildGraph g    -- Crea mapa de fills
    priority x = maybe (length $ getList boys) id (elemIndex x $ getList boys)
    go _ [] path = path               -- Cas base: no queden nodes per visitar
    go visited (n:stack) path         -- Cas recursiu
      | Set.member n visited = go visited stack path  -- Si ja està visitat, salta
      | otherwise =                   -- Si no està visitat
          let visited' = Set.insert n visited  -- Marca com visitat
              children = Map.findWithDefault [] n childrenMap  -- Obté fills
              sorted = reverse $ sortBy (comparing priority) children  -- Ordena fills
          in go visited' (sorted ++ stack) (path ++ [n])  -- Continua recursivament

-- Create a graph excluding the root
createGraph :: Jelouses -> String -> Graph
createGraph jelouses root = Map.delete root jelouses

-- Follow the path from a node to its parent
path :: Graph -> String -> [String]
path graph node = case Map.lookup node graph of
    Nothing -> [node]
    Just next -> node : path graph next

-- Create a priority list for traversal
createPriority :: String -> ListF String -> String -> ListF String -> ListF String
createPriority root unprior friend boys =
    ListF $ base ++ [x | x <- getList boys, x `notElem` base, x `notElem` getList unprior]
  where
    base = [root] ++ [friend | friend /= root]

-- Verifica si la solució és correcta
isCorrect :: [String] -> Jelouses -> Friends -> Bool
isCorrect sol jelouses friends = all check sol
  where
    -- Mapa de posicions dels nodes en la solució
    pos = Map.fromList (zip sol [0..])
    -- Funció per comprovar cada node
    check node = case (Map.lookup node jelouses, Map.lookup node friends) of
        (Just j, Just f) ->
            case (Map.lookup node pos, Map.lookup j pos, Map.lookup f pos) of
                (Just p, Just pj, Just pf) ->
                    -- Comprova les restriccions de gelosia i amistat:
                    -- 1. El gelós ha d'estar després (p < pj)
                    -- 2. L'amic ha d'estar després del gelós o entre els dos
                    p < pj && (pf > pj || (pf > p && pf < pj))
                _ -> True
        _ -> True

-- Detecta cicles al graf
foundCicle :: Graph -> Bool -> [String]
foundCicle graph needRoots = go (Map.keys graph) Set.empty []
  where
    -- Funció auxiliar per recórrer tots els nodes
    go [] _ roots = if needRoots then roots else []
    go (n:ns) visited roots
        | n `Set.member` visited = go ns visited roots  -- Salta nodes ja visitats
        | otherwise =
            let (cycle, visited', r) = explore n Set.empty visited []
            in if cycle && not needRoots then [] else go ns visited' (roots ++ r)

    -- Funció per explorar un camí des d'un node
    explore current path visited acc = case Map.lookup current graph of
        Nothing -> (False, Set.insert current visited, acc ++ [current])
        Just next
            | current `Set.member` path -> (True, visited, acc)  -- Cicle detectat
            | current `Set.member` visited -> (False, visited, acc)  -- Ja visitat
            | otherwise -> explore next (Set.insert current path) 
                                      (Set.insert current visited) 
                                      (acc ++ [current])

-- Trenca cicles al graf
breakCicles :: Graph -> String -> Friends -> (Graph, Bool)
breakCicles graph node friends = go (Map.insert (friends Map.! node) node graph) Set.empty node
  where
    go g seen current
        | foundCicle g False == [] = (g, True)  -- No hi ha cicles
        | show g `Set.member` seen = (g, False)  -- No es pot resoldre el cicle
        | otherwise =
            -- Intenta trencar el cicle modificant les arestes
            let n = friends Map.! current
                g1 = Map.delete n g
                n2 = friends Map.! (friends Map.! n)
                g2 = Map.insert n2 (friends Map.! n) g1
            in go g2 (Set.insert (show g) seen) n

-- Construeix el graf i determina si té solució
buildGraph :: [String] -> Jelouses -> Friends -> String -> (Bool, Graph, [String])
buildGraph boys jelouses friends root =
    let graph = createGraph jelouses root
        jr = jelouses Map.! root     -- Node gelós de l'arrel
        fr = friends Map.! root      -- Amic de l'arrel
        (pr, up) = (path graph fr, path graph jr)  -- Camins des de l'amic i el gelós
        priority = getList $ createPriority root (ListF up) fr (ListF boys)
        (g, ok) = if jr `elem` pr    -- Si el gelós està al camí de l'amic
                  then breakCicles graph root friends  -- Cal trencar cicles
                  else (graph, True)  -- No cal trencar cicles
    in (ok, g, priority)

-- Imprimeix la solució
printSolution :: [String] -> IO ()
printSolution sol = putStrLn $ unwords sol  -- Uneix amb espais i imprimeix

-- Avalua la solució
evaluate :: ListF String -> Jelouses -> Friends -> IO ()
evaluate boys jelouses friends =
    let roots = foundCicle jelouses True  -- Troba possibles arrels
    in try roots
  where
    try [] = putStrLn "impossible"  -- No hi ha arrels vàlides
    try (r:rs) =
        let (ok, graph, prio) = buildGraph (getList boys) jelouses friends r
        in if not ok 
           then try rs  -- Prova amb la següent arrel
           else let sol = dfs graph r (ListF prio)
                in if isCorrect (getList sol) jelouses friends 
                   then printSolution (getList sol)  -- Solució trobada
                   else try rs  -- Prova amb la següent arrel

-- Funció principal
main :: IO ()
main = do
    args <- getArgs  -- Obté arguments de la línia de comandes
    case args of
        (file:_) -> do  -- Si hi ha almenys un argument
            (boys, jelouses, friends) <- illa file  -- Llegeix el fitxer
            evaluate boys jelouses friends  -- Avalua la solució
        _ -> putStrLn "Usage: ./program <input_file>"  -- Si no hi ha arguments
