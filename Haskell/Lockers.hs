import Data.Foldable
import qualified Data.Map.Lazy as Map

type Lockers = Map.Map Int Bool
               
makeLockers :: Int -> Lockers
makeLockers n = Map.fromList $ zip [1..n] $ replicate n False

changeLockersState :: Int -> Lockers -> Lockers
changeLockersState n l = foldr' (Map.adjust not) l modNList
  where modNList = [ n * m | m <- take (Map.size l `quot` n) [1..] ]

runLockers :: Lockers -> Lockers
runLockers l = foldr' changeLockersState l [1..Map.size l]

main :: IO ()
main = do
  putStrLn "Enter how many lockers: "
  n <- fmap read getLine :: IO Int
  let lockers = runLockers $ makeLockers n

  putStrLn $ "The following lockers remained open after " ++ show n ++ " iterations: "
  print (Map.keys $ Map.filterWithKey (flip const) lockers)
