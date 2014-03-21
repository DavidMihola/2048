import Data.List (transpose, intercalate)
import System.IO (hSetBuffering, stdin, BufferMode(..))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.Random (randomRIO)

type Line = [Int]
type Board = [Line]

-- pull the line together as defined below and fill with zeros to
-- a length of 4
pullLine :: [Int] -> [Int]
pullLine xs = take 4 $ (pullLine'' xs) ++ repeat 0

-- pullLine' and pullLine'' should be equivalent:
-- note that [2,0,2,2] is pulled togehter as [4,2,0,0]
-- and not as [2,4,0,0], i.e. if more than two adjacent
-- cells are in a row the pairs are collapsed in a
-- left-associative way, even if there is zero in between
pullLine' :: [Int] -> [Int]
pullLine' [] = []
pullLine' [x] = [x]
pullLine' (x:y:xs) =  
  if (x == 0)
  then pullLine' (y:xs)
  else if (y == 0)
       then pullLine' (x:xs)
       else if (x == y)
            then (x + y) : pullLine' (xs)
            else x : pullLine' (y:xs)

pullLine'' :: Line -> Line
pullLine'' = collapsePairs . removeZeros
  where removeZeros = filter (/= 0)
        collapsePairs [] = []
        collapsePairs [x] = [x]
        collapsePairs (x:y:xs) = if (x == y)
                                 then (x + y) : (collapsePairs xs)
                                 else x : collapsePairs (y:xs)

-- to pull the whole board pull each inner list 
pull :: Board -> Board
pull = map pullLine

-- left is the standard direction (when a list of lists is interpreted
-- as a list of rows)
pullLeft = pull

-- to pull right: reverse each row, pull left and reverse again
pullRight = (map reverse) . pull . (map reverse)

-- to pull up: transpose the matrix (so that columns are now rows),
-- pull left and transpose again (so that rows are now columns again)
pullUp = transpose . pull . transpose

-- to pull down: combine the transformations of pullUp and pullRight
-- unfortunately the order of transformations must be reversed after
-- the pull; that's pity because both reverse . reverse = id and
-- transpose . transpose = id, but I can't think of a transformation
-- for pullDown that would also be reversible
pullDown = (transpose . map reverse) . pull . (map reverse . transpose)


-- some helper functions for formatted output

-- take a string s and add spaces to the left until the whole string is
-- n characters long
leftPadStringTo n s = reverse $ take n (reverse s ++ repeat ' ')

-- to format a line: pad each cell to 4 characters and put pipe between
-- each pair of cells
formatLine line = intercalate "|" $ map (leftPadStringTo 4 . show) line

-- to format a board: format each line and put an empty line between
-- each pair of lines
formatBoard board = intercalate "\n" $ map formatLine board


-- add a 2 to a randomly chosen empty (zero) cell of the board
addRandomNumber :: Board -> IO Board
addRandomNumber board = do
  coordinates <- pickRandom $ findEmptyCells board
  let newValue = 2
  return $ placeNumber newValue coordinates board

-- return a list of coordinates of all empty (zero) cells of the board
findEmptyCells :: Board -> [(Int, Int)]
findEmptyCells board = filter (\(row, col) -> (board !! row !! col) == 0) allCoordinates
  where allCoordinates = [(row, col) | row <- [0..3], col <- [0..3]]

-- take a board and replace the content of (row, col) with n
placeNumber :: Int -> (Int, Int) -> Board -> Board
placeNumber n (row, col) board = replaceElement board row newRow
  where newRow = replaceElement (board !! row) col n

-- take a list of xs and replace the i'th element with x
replaceElement xs i x = before ++ (x : after)
  where before = take i xs
        after = drop (i+1) xs

-- pick a random element from a list
pickRandom :: [a] -> IO a
pickRandom xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

-- two boards: a partially filled one for testing
--             and an empty one for the actual game
testBoard :: Board 
testBoard = [[0,2,0,0],
             [2,2,2,2],
             [2,4,0,2],
             [8,0,8,8]]

emptyBoard :: Board
emptyBoard = take 4 $ repeat $ take 4 $ repeat 0

-- main entry point to the program
-- set stdin to NoBuffering or else every command would need to be
-- followed by enter
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  startBoard <- (addRandomNumber emptyBoard >>= addRandomNumber)
  loop startBoard

commandMap :: Map Char (Board -> Board)
commandMap = Map.fromList [('l', pullRight),
                           ('j', pullLeft),
                           ('k', pullDown),
                           ('i', pullUp)]

loop :: Board -> IO ()
loop board = do
  putStrLn $ "\n" ++ formatBoard board
  c <- getChar
  if (c == 'q')
  then return ()
  else if (Map.member c commandMap)
       then do
         putChar c
         let newBoard = (commandMap ! c) board
         if (newBoard == board)
         then loop newBoard 
         else do
           randomBoard <- addRandomNumber newBoard
           loop $ randomBoard
       else do
         putStrLn "Unknown command"
         loop board
