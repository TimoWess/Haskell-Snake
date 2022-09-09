module Main where

import Data.List.Split (chunksOf)

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
        (width, height, replaceNth 6 Food ((replicate (width * height) Empty)))

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

updateSnake :: Game -> Snake
updateSnake (board, (dir, body)) =
    (dir, (applyMove board dir (head body)) : (init body))

growSnake :: Game -> Snake
growSnake (board, (dir, body)) = (dir, (applyMove board dir (head body)) : body)

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

update :: Game -> IO ()
update (board@(Board (width, height, spaces)), snake) = do
    print $ positionSnakeOnBoard board snake
    let newSnake@(dir, snakeHead@(x, y):body) = updateSnake (board, snake)
        headPosition = partPosition snakeHead
    if x < 0 || y < 0 || x >= width || y >= height
        then fail "Snake hit wall"
        else if (spaces !! headPosition) == Body
                 then fail "Snake hit body"
                 else if (spaces !! headPosition) == Food
                          then update
                                   ( Board
                                         ( width
                                         , height
                                         , replaceNth headPosition Empty spaces)
                                   , (growSnake (board, snake)))
                          else update (board, (newSnake))
