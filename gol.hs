module Main where
import Data.List
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Foldable (fold)
import Data.Function

type Board = [Cell]
type Coordinate = (Int, Int)
data Cell = Alive Coordinate
          | Dead Coordinate

instance Show Cell where
  show (Alive _) = "X"
  show (Dead _) = "O"

newBoard :: Int -> Int -> Board
newBoard x y = [Dead (x',y') | x' <- [0..(x - 1)], y' <- [0..(y - 1)]]

showBoard :: Board -> String
showBoard = fold . intersperse "\n" . map (fold . map show) . rows

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (compare `on` f)

rows :: Board -> [[Cell]]
rows = groupBy (\a b -> snd (coord a) == snd (coord b)) . sortWith (snd . coord)

coord :: Cell -> Coordinate
coord (Alive c) = c
coord (Dead c) = c

lookupCell :: Board -> Coordinate -> Cell
lookupCell board (x, y) = let matching = filter ((==)(x,y) . coord) board
                          in if length matching == 1 then head matching else Dead (x, y)

neighboringCoords :: Coordinate -> [Coordinate]
neighboringCoords (x, y) = [(x',y') | x' <- [x+1, x, x-1], y' <- [y+1, y, y-1], (x,y) /= (x',y')]

neighbors :: Board -> Coordinate -> [Cell]
neighbors board coordinate = map (lookupCell board) $ neighboringCoords coordinate

isAlive :: Cell -> Bool
isAlive (Alive _) = True
isAlive (Dead _) = False

nextCell :: Board -> Cell -> Cell
nextCell board cell = case cell of
                        (Alive c) -> case length (aliveNeighbors c) of
                          n | n `elem` [2..3] -> Alive c
                          _ -> Dead c
                        (Dead c) -> if length (aliveNeighbors c) == 3 then Alive c else Dead c
  where aliveNeighbors coordinate = filter isAlive $ neighbors board coordinate

nextBoard :: Board -> Board
nextBoard board = map (nextCell board) board

place :: Cell -> Board -> Board
place cell = let newCoord = coord cell
             in map (\old -> if coord old == newCoord then cell else old)

every :: (Monad m, MonadIO m) => Float -> m a -> m ()
every secs action = do _ <- action
                       liftIO . threadDelay . floor $ secs * 1000000.0
                       every secs action

foreverEvolveAndPrint :: Board -> IO ()
foreverEvolveAndPrint board = void $ runStateT infiniteLoop board
  where infiniteLoop = every 0.5 $ getAndEvolveBoard >>= (liftIO . putStrLn . (++ "\n") . showBoard)

getAndEvolveBoard :: StateT Board IO Board
getAndEvolveBoard = do
  current <- get
  put $ nextBoard current
  return current

main :: IO ()
main = foreverEvolveAndPrint board
  where width = 10
        height = 10
        board = place (Alive (2,2))
              . place (Alive (2,3))
              . place (Alive (1,2))
              . place (Alive (1,3))
              . place (Alive (4,1))
              $ newBoard width height
