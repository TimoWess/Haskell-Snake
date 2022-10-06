module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.List.Split (chunksOf)
import System.Console.ANSI
import System.IO
import System.Process (system)
import System.Random

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    let width = 30
        height = 30
        initGame = (initBoard width height, initSnake)
    startingGame@(board, _) <- generateFood initGame >>= generateFood
    system "clear"
    print board
    update startingGame board

data Direction
    = U
    | R
    | D
    | L
    deriving (Enum, Show, Eq, Ord)

type Snake = (Direction, [Part])

getHead :: Snake -> Part
getHead (_, (x:xs)) = x

type Part = (Int, Int)

newtype Board =
    Board (Int, Int, [Space])

getSpaces :: Board -> [Space]
getSpaces (Board (_, _, s)) = s

instance Show Board where
    show (Board (width, height, spaces)) =
        (unlines . map unwords . chunksOf width . map show) spaces

data Space
    = Empty
    | Food
    | Body
    | Head Direction
    | Wall
    deriving (Eq)

instance Show Space where
    show Empty = " "
    show Food = "*"
    show Body = "+"
    show Wall = "#"
    show (Head U) = "^"
    show (Head R) = ">"
    show (Head D) = "v"
    show (Head L) = "<"

type Game = (Board, Snake)

initBoard :: Int -> Int -> Board
initBoard width height =
    Board
        ( width
        , height
        , (replicate width Wall ++ concat (replicate (height - 2) innerSegment)) ++
          replicate width Wall)
  where
    innerSegment = [Wall] ++ replicate (width - 2) Empty ++ [Wall]

initSnake :: Snake
initSnake = (R, [(3, 1), (2, 1), (1, 1)])

setDir :: Snake -> Direction -> Snake
setDir (dir, parts) newDir = (newDir, parts)

positionSnakeOnBoard :: Game -> Board
positionSnakeOnBoard (board@(Board (width, height, spaces)), snake@(dir, parts)) =
    let lst = snakePositionsMap (board, snake)
     in Board
            ( width
            , height
            , foldr
                  (\(space, index) akk ->
                       let isSnake = lookup index lst
                        in (if maybe False id isSnake
                                then if index ==
                                        (\(x, y) -> x + y * width) (head parts)
                                         then Head dir
                                         else Body
                                else space) :
                           akk)
                  []
                  (zip spaces [0 ..]))

directionToMove :: Direction -> (Int, Int)
directionToMove dir =
    case dir of
        U -> (0, -1)
        R -> (1, 0)
        D -> (0, 1)
        L -> (-1, 0)

applyMove :: Board -> Direction -> Part -> Part
applyMove board@(Board (width, height, _)) dir (x, y) = (x + dx, y + dy)
  where
    (dx, dy) = directionToMove dir

updateSnake :: Game -> Bool -> Snake
updateSnake (board, (dir, body)) grow =
    ( dir
    , (applyMove board dir (head body)) :
      (if grow
           then body
           else init body))

changeDirection :: Snake -> Int -> Snake
changeDirection (currentDir, body) change =
    let newDir =
            toEnum
                (if dirNum < 0
                     then 3
                     else dirNum) :: Direction
        dirNum = (fromEnum currentDir + change) `mod` 4
     in (newDir, body)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal : xs
    | otherwise = x : replaceNth (n - 1) newVal xs

-- partPosition needs the width of the board as it's first argument
partPosition :: Int -> Part -> Int
partPosition width (x, y) = x + y * width

snakePositionsMap :: Game -> [(Int, Bool)]
snakePositionsMap ((Board (width, height, _)), (_, body)) =
    map (\(x, y) -> (x + y * width, True)) body

isOutOfBounds :: Game -> Bool
isOutOfBounds ((Board (width, height, _)), (_, (x, y):_)) =
    x < 0 || y < 0 || x >= width || y >= height

generateFood :: Game -> IO Game
generateFood (board@(Board (width, height, spaces)), snake@(dir, parts@(head:body))) = do
    g <- newStdGen
    let position = fst $ randomR (0, width * height - 1) g
        headPosition = partPosition width head
        fullSpaces = getSpaces $ positionSnakeOnBoard (board, (dir, init parts))
        matchPosition = fullSpaces !! position
    if matchPosition == Empty
        then return $
             ( Board
                   ( width
                   , height
                   , (replaceNth position Food . replaceNth headPosition Empty)
                         spaces)
             , snake)
        else generateFood (board, snake)

redraw :: Board -> Board -> IO ()
redraw currentBoard@(Board (_, _, currentSpaces)) nextBoard@(Board (width, _, spaces)) = do
    let zippedSpaces = zip currentSpaces spaces
        indexedSpaces = zip [0 ..] zippedSpaces
    forM_ indexedSpaces $ \(index, (currentSpace, nextSpace)) -> do
        let (y, x) = index `divMod` width
        when (currentSpace /= nextSpace) $ do
            setCursorPosition y (x * 2) -- x*2 since every horizontal space is two characters
            putStr $ show nextSpace
            setCursorPosition 0 0
            hFlush stdout

newDir :: Direction -> String -> Direction
newDir dir i
    | head i == 'a' = toEnum $ m4 (dirNum - 1 + 4)
    | head i == 'd' = toEnum $ m4 (dirNum + 1)
    | otherwise = dir
  where
    dirNum = fromEnum dir
    m4 = (`mod` 4)

update :: Game -> Board -> IO ()
update (board@(Board (width, height, spaces)), snake@(dir, parts)) lastBoard = do
    let displayBoard = positionSnakeOnBoard (board, snake)
    redraw lastBoard displayBoard
    isInput <- hWaitForInput stdin 100
    input <- getInput isInput
    let newSnakeGen = updateSnake (board, setDir snake nextDir)
        nextDir = newDir dir [input]
        movedSnake = newSnakeGen False
        grownSnake = newSnakeGen True
        headPosition = partPosition width (getHead $ movedSnake)
        fullSpaces = getSpaces $ positionSnakeOnBoard (board, (dir, init parts))
        spaceAtHead = fullSpaces !! headPosition
    boardWithNewFood <- generateFood (board, grownSnake)
    case spaceAtHead of
        Wall -> system "clear" >> fail "Snake hit wall"
        Body -> system "clear" >> fail "Snake hit body"
        Empty -> update (board, movedSnake) displayBoard
        Food -> update boardWithNewFood displayBoard

getInput :: Bool -> IO Char
getInput True = getChar
getInput False = return ' '
