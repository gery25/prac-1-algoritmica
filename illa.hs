
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Error: Please provide a filename as argument"
        else do
            contents <- readFile (head args)
            mapM_ (putStrLn . take 1) $ filter (not . null) $ lines contents