module CheckersBoardInstance(
    Checkers(Man,King,Empty),
    startBoard''
)where
import GeneralGame
import CoordinateFunctions
import GeneralBoardFunctions

-----------------------------
-- ** TypesAndNiceWriting ** --
-----------------------------
type IsWhite = Bool
type Coordinate = String
white = True
black = False
---------------------------------
-- ** DataDeclareAndGeneral ** --
---------------------------------

data Checkers = Man Bool | King Bool | Empty deriving Eq

instance BoardGame Checkers where
    newInputBoard = newInputBoard''
    startBoard = startBoard''
    currentPlayerHasWon = currentPlayerHasWon''
    gameIsATie = gameIsATie''
    printBoard = printBoardFunc
    empty = Empty
    isWhite = isWhite''

instance Show Checkers where
    show (Man _) = "M"
    show (King _) = "K"
    show Empty = " "

-------------------
-- ** isWhite ** --
-------------------
isWhite'' :: Checkers -> Bool
isWhite'' (Man x) = x
isWhite'' (King x) = x
--------------------
-- **StartBoard** --
--------------------
startBoard'' = Board [[Empty, Man white, Empty, Man white, Empty, Man white, Empty, Man white],
                      [Man white, Empty, Man white, Empty, Man white, Empty, Man white, Empty],
                      [Empty, Man white, Empty, Man white, Empty, Man white, Empty, Man white],
                      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                      [Man black, Empty, Man black, Empty, Man black, Empty, Man black, Empty],
                      [Empty, Man black, Empty, Man black, Empty, Man black, Empty, Man black],
                      [Man black, Empty, Man black, Empty, Man black, Empty, Man black, Empty]]
---------
gameIsATie'' currentBoard = False
currentPlayerHasWon'' currentBoard = False
aiMovement currentBoard isFirstPlayersTurn aiMove = return currentBoard
--------------------
-- **MovePieces** --
--------------------
newInputBoard'' currentBoard isFirstPlayersTurn mainChoise aiMove =
    if mainChoise == 1 || (isFirstPlayersTurn && mainChoise == 2) || (isFirstPlayersTurn ==False && mainChoise == 3) then do
        putStrLn "Choose which piece to move: "
        playerMovePiece currentBoard isFirstPlayersTurn
    else do
        aiMovement currentBoard isFirstPlayersTurn aiMove


playerMovePiece currentBoard isFirstPlayersTurn = do
    movePiece <- getInputAndDemandFormat currentBoard
    movePieceFromPosition movePiece currentBoard isFirstPlayersTurn

movePieceFromPosition "q" _ _ = return deadBoard
movePieceFromPosition movePiece currentBoard isFirstPlayersTurn
    | elem movePiece (positionsWithMovablePieces currentBoard isFirstPlayersTurn) == False
        = do putStrLn "That piece is not possible to move. Give new Input: "
             playerMovePiece currentBoard isFirstPlayersTurn
    | otherwise
        = do putStrLn "Move to position: "
             movePieceTo movePiece currentBoard isFirstPlayersTurn

movePieceTo movePiece currentBoard isFirstPlayersTurn = do
    moveTo <- getInputAndDemandFormat currentBoard
    movePieceToInput moveTo movePiece currentBoard isFirstPlayersTurn

movePieceToInput "b" _ currentBoard isFirstPlayersTurn = do
    putStrLn "Choose which piece to move:"
    playerMovePiece currentBoard isFirstPlayersTurn
movePieceToInput "q" _ _ _ = return deadBoard
movePieceToInput moveTo movePiece currentBoard isFirstPlayersTurn
    | elem moveTo (legalMovesForPiece movePiece currentBoard isFirstPlayersTurn) == False
        = do
            putStrLn "That piece is not possible to move there. Give new Input: "
            movePieceTo movePiece currentBoard isFirstPlayersTurn
    | abs (convToXCord moveTo - convToXCord movePiece) == 1
            = do
                let newTempBoard = changePieceTo currentBoard moveTo (getPieceAtPosition currentBoard movePiece)
                let newBoard = changePieceTo newTempBoard movePiece empty
                return newBoard -----FORCE TO MOVE AGAIN????
    | otherwise
            = do
                putStrLn "BBBB"
                let newTempBoard = changePieceTo currentBoard moveTo (getPieceAtPosition currentBoard movePiece)
                let newTempBoard'' = changePieceTo currentBoard (middleCoordinate moveTo movePiece) empty
                let newBoard = changePieceTo newTempBoard'' movePiece empty
                return newBoard -----FORCE TO MOVE AGAIN????



-- Pieces On The Board --
--emptyBoardPositions
legalMovesForPiece :: Coordinate -> Board Checkers -> IsWhite -> [Coordinate]
legalMovesForPiece coordinate currentBoard isFirstPlayersTurn = plainLegalMoves currentBoard coordinate----- CHANGE THIS


positionsWithMovablePieces :: Board Checkers -> IsWhite -> [String]
positionsWithMovablePieces (Board mList) itsFirstPlayersTurn = filter (isPieceMovable mList itsFirstPlayersTurn) (boardPositions (Board mList))

isPieceMovable mList itsFirstPlayersTurn coordinate =
    let piece = getPieceAtPosition (Board mList) coordinate
        in piece /= Empty &&
           isWhite piece == itsFirstPlayersTurn


--normalLegalMovesOnBoard :: Board Checkers ->
plainLegalMoves inBoard coordinate =
    filter (\x -> elem x (emptyPositions inBoard)) (generalPiecePossibleMoves (getPieceAtPosition inBoard coordinate) coordinate)

generalPiecePossibleMoves :: Checkers -> (Coordinate -> [Coordinate])
generalPiecePossibleMoves piece coordinate
    | piece == Man True                   =[modifyCoordinate coordinate (1) (1), modifyCoordinate coordinate (-1) (1)]
    | piece == Man False                  =[modifyCoordinate coordinate (1) (-1), modifyCoordinate coordinate (-1) (-1)]
    | piece /= Empty                      =[modifyCoordinate coordinate (-1) (-1), modifyCoordinate coordinate (-1) (1),
                                            modifyCoordinate coordinate (1) (-1), modifyCoordinate coordinate (1) (1) ]

{-


getStartPos
ifNotQOrBad  then getEndPos
ifAllowedEndPos(forcedTake?) modifyBoard then lookIfMovedPieceHasMoreTakes



--------------------
-- **PrintBoard** --
--------------------

-}
