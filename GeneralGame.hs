module GeneralGame(
    boardGameMain,
    Board(Board),
    BoardGame(newInputBoard,startBoard,currentPlayerHasWon,gameIsATie,printBoard,empty, isWhite),
    printBoardFunc,
    deadBoard,
    maxRowSize,
    printABCreference,
)where
import Data.Char (ord, chr)
import CoordinateFunctions
import System.Console.ANSI

------------------
-- ############ --
------------------
newtype Board a = Board [[a]] deriving Eq

instance (Show a) => Show (Board a) where
    show (Board mList) =
            printABCreference (maxRowSize mList) ++
            printRows mList

instance Functor Board where
    fmap f (Board mList) = Board (map (map f) mList)


class (Eq a, Show a) => BoardGame a where
    newInputBoard :: Board a -> IsFirstPlayersTurn -> ChoiseFromMain -> AI a -> IO (Board a)
    startBoard :: Board a
    currentPlayerHasWon :: Board a -> Bool
    gameIsATie :: Board a -> Bool
    printBoard :: Board a -> IO ()
    empty :: a
    isWhite :: a -> Bool

deadBoard = Board [[]]
------------------
-- ############ --
------------------
type IsFirstPlayersTurn = Bool
type MoveInputString = String
type ChoiseFromMain = Int
type AI a = Board a -> IsFirstPlayersTurn -> String
------------------
-- ############ --
------------------

boardGameMain :: (BoardGame a) => AI a -> IO ()
boardGameMain aiMovement = do
    let running = True
    let itsCrossTurn = True
    let currentBoard = startBoard
    mainChoise <- mainMenu
    if mainChoise == "q" then return ()
        else do
          gameLoop currentBoard itsCrossTurn running (ord (head mainChoise)-ord '0') aiMovement
          boardGameMain aiMovement

gameLoop :: (BoardGame a) => Board a -> Bool -> Bool -> Int -> AI a-> IO ()
gameLoop currentBoard running itsCrossTurn mainChoise aiMovement
    | running == False = do
                if currentBoard == deadBoard then
                    return ()
                    else
                    printBoard currentBoard
                return ()
    | otherwise = do
                putStrLn $ outputTurn itsCrossTurn
                printBoard currentBoard
                newBoard <- newInputBoard currentBoard itsCrossTurn mainChoise aiMovement
                let areThreeNewRow = currentPlayerHasWon newBoard
                let isTie = gameIsATie newBoard
                let running = if (newBoard == deadBoard ||  areThreeNewRow || isTie) then False else True
                if newBoard /= deadBoard then do
                     putStr $ if areThreeNewRow then (currentMark itsCrossTurn ++ " has won! \n") else ""
                     putStr $ if isTie then "\nThe game is a tie!\n" else ""
                   else
                     return ()
                let itsCrossTurn' = ((newBoard == currentBoard) == itsCrossTurn)
                gameLoop newBoard running itsCrossTurn' mainChoise aiMovement

outputTurn itsCrossTurn
    | itsCrossTurn = "\nIt is X turn to move. \n"
    | otherwise    = "\nIt is 0 turn to move. \n"

currentMark True = "X"
currentMark _ = "0"

---------------------
-- ## Main Menu ## --
---------------------

mainMenu = do
   putStrLn $ "\n------------------------ \n" ++
              "MAIN MENUE\n"             ++
              "Please Choose Game Mode\n"++
              "------------------------ " ++
              " \n 1: Player vs Player \n 2: Player vs AI \n 3: AI vs Player \n 4: AI vs AI \n q: To quit \n------------------------ \n"
   input <- getLine
   if input == "q" || input == "1" || input == "2" || input == "3" ||input == "4"
       then return input
    else do
        putStrLn "Bad Input!\n"
        mainMenu

----------------------------------
-- ## General Print functions## --
----------------------------------
maxRowSize :: [[a]] -> Int
maxRowSize xs = (maximum $ map length xs) -1
printABCreference :: Int -> String
printABCreference x = "  " ++ printABCreference'' x 0
printABCreference'' 0 y = [chr (ord 'A' + y)] ++ "\n"
printABCreference'' x y = [chr (ord 'A' + y), ' '] ++ printABCreference'' (x-1) (y+1)

printRows mList = printRows'' mList 1
printRows'' [] x = ""
printRows'' mList x = printRow (head mList) x ++ printRows'' (tail mList) (x+1)
printRow rowList x = show x ++ " "++ foldl (\acc y -> acc ++ show y ++ " ") "" rowList ++ " \n"

printBoardFunc :: (BoardGame a) => Board a -> IO ()
printBoardFunc (Board mList) = do
    putStr (printABCreference $ maxRowSize mList)
    sequence $ createRowIO mList
    return ()

createRowIO mList = createRowIO'' mList 1
createRowIO'' [] x = []
createRowIO'' mList x = [printRowIO (head mList) x] ++ createRowIO'' (tail mList) (x+1)
printRowIO row x = do putStr (show x)
                      sequence $ map colourPutStr row
                      putStrLn " "

colourPutStr :: (BoardGame a) => a -> IO ()
colourPutStr piece = do
     putStr " "
     if piece == empty then return () else
         if (isWhite piece) then return () else setSGR[ SetConsoleIntensity BoldIntensity
             , SetColor Foreground Dull Red
             ]
     putStr (show piece)
     setSGR [ Reset ]
-----------------------------------
-- ## General Board Functions ## --
-----------------------------------

