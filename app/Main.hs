module Main where

import Control.Concurrent (threadDelay)
import Data.List.Split (chunksOf)
import System.Process (system)
import System.Random

main :: IO ()
main = do
    let board = initBoard gWidth gHeight
        snake = initSnake
    update (board, snake)

gWidth :: Int
gWidth = 20

gHeight :: Int
gHeight = 20

data Direction
    = U
    | R
    | D
    | L
    deriving (Enum, Show, Eq, Ord)

type Snake = (Direction, [Part])

type Part = (Int, Int)

newtype Board =
    Board (Int, Int, [Space])

instance Show Board where
    show (Board (width, height, spaces)) =
        replicate (width * 2 + 1) '-' ++
        "\n" ++
        (unlines .
         map (\i -> "|" ++ i ++ "|") . map unwords . chunksOf width . map show)
            spaces ++
        replicate (width * 2 + 1) '-'

data Space
    = Empty
    | Food
    | Body
    | Head
    deriving (Eq)

instance Show Space where
    show Empty = " "
    show Food = "*"
    show Body = "+"
    show Head = "#"

type Game = (Board, Snake)

initBoard :: Int -> Int -> Board
initBoard width height =
    Board
        ( width
        , height
        , (replaceNth 9 Food . replaceNth 6 Food)
              ((replicate (width * height) Empty)))

initSnake :: Snake
initSnake = (R, [(2, 0), (1, 0), (0, 0)])

positionSnakeOnBoard :: Board -> Snake -> Board
positionSnakeOnBoard board@(Board (width, height, spaces)) snake@(_, parts) =
    let lst = snakePositionsMap board snake
     in Board
            ( width
            , height
            , foldr
                  (\(space, index) akk ->
                       let isSnake = lookup index lst
                        in (if maybe False id isSnake
                                then if index ==
                                        (\(x, y) -> x + y * width) (head parts)
                                         then Head
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

partPosition :: Part -> Int
partPosition (x, y) = x + y * gWidth

snakePositionsMap :: Board -> Snake -> [(Int, Bool)]
snakePositionsMap (Board (width, height, _)) (_, body) =
    map (\(x, y) -> (x + y * width, True)) body

isOutOfBounds :: Game -> Bool
isOutOfBounds ((Board (width, height, _)), (_, (x, y):_)) =
    x < 0 || y < 0 || x >= width || y >= height

update :: Game -> IO ()
update (board@(Board (width, height, spaces)), snake) = do
    system "clear"
    print $ positionSnakeOnBoard board snake
    threadDelay 100000
    g <- newStdGen
    let newSnake@(dir, snakeHead@(x, y):body) = updateSnake (board, snake) False
        headPosition = partPosition snakeHead
        spaceAtHead = spaces !! headPosition
        gotFood = spaceAtHead == Food
    if isOutOfBounds (board, newSnake)
        then fail "Snake hit wall"
        else if (spaceAtHead) == Body
                 then fail "Snake hit body"
                 else update
                          ( Board
                                ( width
                                , height
                                , ((if gotFood
                                        then replaceNth
                                                 (fst $
                                                  randomR
                                                      (0, width * height - 1)
                                                      g)
                                                 Food
                                        else id) .
                                   replaceNth headPosition Empty)
                                      spaces)
                          , (updateSnake (board, snake) gotFood))
