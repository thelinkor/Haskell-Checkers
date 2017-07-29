module GeneralBoardFunctions(
    emptyPositions,
    getInputAndDemandFormat,
    isInputLegal,
    getPieceAtPosition,
    boardPositions,
    changePieceTo,
    allPiecesOfCurrentTurn
)where
import GeneralGame
import CoordinateFunctions

-------------------------
-- **Empty Positions** --
-------------------------

emptyPositions :: (BoardGame a) => Board a -> [String]
emptyPositions (Board markList)  =
    emptyPositions' markList 0

emptyPositions' [] _ = []
emptyPositions' markList y = (emptyYPositions (head markList) 0 y) ++ (emptyPositions' (tail markList) (y+1))

emptyYPositions [] _ _ = []
emptyYPositions markRow x y
    | head markRow /= empty = emptyYPositions (tail markRow) (x+1) y
    | otherwise             = [[convFromXCord x]++[convFromYCord y]] ++ emptyYPositions (tail markRow) (x+1) y


--------------------------
-- **Board Positions**  --
--------------------------
boardPositions :: Board a -> [String]
boardPositions (Board mList) = boardPositions'' 0 mList
boardPositions'' x [] = []
boardPositions'' x mList = (boardYPositions (convFromXCord x) 0 (head mList)) ++ boardPositions'' (x+1) (tail mList)
boardYPositions xcord y [] = []
boardYPositions xcord y hmList = [[xcord]++[convFromYCord y]]++boardYPositions xcord (y+1) (tail hmList)

allPiecesOfCurrentTurn :: (BoardGame a) => Board a -> Bool -> [String]
allPiecesOfCurrentTurn currentBoard itsFirstPlayersTurn =
    filter (\x ->  (getPieceAtPosition currentBoard x) /=empty &&
                isWhite(getPieceAtPosition currentBoard x) == itsFirstPlayersTurn) (boardPositions currentBoard)

------------------------
-- **InputFormating** --
------------------------

getInputAndDemandFormat :: (BoardGame a) => Board a -> IO String
getInputAndDemandFormat inputBoard = do
    input <- getLine
    checkInput inputBoard input

checkInput inputBoard "q" = return "q"
checkInput inputBoard input
    | isInputLegal inputBoard input = return input
    | otherwise                     = badInputRequestNew inputBoard

badInputRequestNew inputBoard = do
    putStrLn "Bad input, Please give new input: "
    getInputAndDemandFormat inputBoard
-----

isInputLegal :: Board a -> String -> Bool
isInputLegal (Board mList) input
    | length input /= 2 = False
    | x < 0 || y<0 || x>(maxRowSize mList) || y>(length mList) = False
    | otherwise = True
    where
      x =convToXCord input
      y =convToYCord input


-----------------------------
-- **Board Modifications** --
-----------------------------

getPieceAtPosition :: Board a -> String -> a
getPieceAtPosition (Board mList) coordinate =mList !! (convToYCord coordinate) !! (convToXCord coordinate)
---

changePieceTo :: Board a -> String -> a -> Board a
changePieceTo (Board mList) coordinate changeTo =
    Board (changeYPieceTo mList (convToXCord coordinate) (convToYCord coordinate) changeTo)

changeYPieceTo mList x 0 changeTo = [changeXPieceTo (head mList) x changeTo] ++ tail mList
changeYPieceTo mList x y changeTo = [head mList] ++ changeYPieceTo (tail mList) x (y-1) changeTo

changeXPieceTo xmList 0 changeTo = [changeTo] ++ tail xmList
changeXPieceTo xmList x changeTo = [head xmList] ++ changeXPieceTo (tail xmList) (x-1) changeTo
