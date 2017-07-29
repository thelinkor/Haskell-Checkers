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

gameIsATie''  currentBoard = False
aiMovement currentBoard isFirstPlayersTurn aiMove = return currentBoard
--------------------
-- **Tie Game**   --
--------------------

currentPlayerHasWon'' currentBoard = allPiecesOfCurrentTurn currentBoard True == [] ||
                            allPiecesOfCurrentTurn currentBoard False == []

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
                let lastBoard = promoteToKing newBoard moveTo isFirstPlayersTurn
                return newBoard
    | otherwise
            = do
                let newTempBoard = changePieceTo currentBoard moveTo (getPieceAtPosition currentBoard movePiece)
                let newTempBoard'' = changePieceTo newTempBoard (middleCoordinate moveTo movePiece) empty
                let newBoard = changePieceTo newTempBoard'' movePiece empty
                let lastBoard = promoteToKing newBoard moveTo isFirstPlayersTurn
                if (takeMovesFromPosition lastBoard moveTo isFirstPlayersTurn == []) then
                        do return lastBoard
                    else do
                        printBoard lastBoard
                        putStrLn "New take move possible. Move to position: "
                        movePieceTo moveTo lastBoard isFirstPlayersTurn


----- King

promoteToKing currentBoard moveTo isFirstPlayersTurn
    | isFirstPlayersTurn && yCord== 7   = changePieceTo currentBoard moveTo (King isFirstPlayersTurn)
    | yCord == 0                        = changePieceTo currentBoard moveTo (King isFirstPlayersTurn)
    | otherwise                         = currentBoard
    where
        xCord = convToYCord(moveTo)
-- Pieces On The Board --
--emptyBoardPositions

-- First called when we want to start moving
positionsWithMovablePieces :: Board Checkers -> IsWhite -> [String]
positionsWithMovablePieces (Board mList) itsFirstPlayersTurn
    | takeMovesTotal /= [] = takeMovesTotal
    | otherwise            = filter (isPieceMovable mList itsFirstPlayersTurn) (boardPositions (Board mList))
    where
        takeMovesTotal = piecesWithTakeMoves (Board mList) itsFirstPlayersTurn

legalMovesForPiece :: Coordinate -> Board Checkers -> IsWhite -> [Coordinate]
legalMovesForPiece coordinate currentBoard isFirstPlayersTurn
    | takemoves /= [] = takemoves
    | otherwise       = plainLegalMoves currentBoard coordinate
    where
        takemoves = takeMovesFromPosition currentBoard coordinate isFirstPlayersTurn

isPieceMovable mList itsFirstPlayersTurn coordinate =
    let piece = getPieceAtPosition (Board mList) coordinate
        in piece /= Empty &&
           isWhite piece == itsFirstPlayersTurn &&
           legalMovesForPiece coordinate (Board mList) itsFirstPlayersTurn /= []


plainLegalMoves inBoard coordinate =
    filter (\x -> elem x (emptyPositions inBoard)) (generalPiecePossibleMoves (getPieceAtPosition inBoard coordinate) coordinate)

generalPiecePossibleMoves :: Checkers -> (Coordinate -> [Coordinate])
generalPiecePossibleMoves piece coordinate = piecePossibleMoves piece coordinate 1

piecePossibleMoves piece coordinate i
    | piece == Man True                   =[modifyCoordinate coordinate (i) (i), modifyCoordinate coordinate (-i) (i)]
    | piece == Man False                  =[modifyCoordinate coordinate (i) (-i), modifyCoordinate coordinate (-i) (-i)]
    | piece /= Empty                      =[modifyCoordinate coordinate (-i) (-i), modifyCoordinate coordinate (-i) i,
                                            modifyCoordinate coordinate (i) (-i), modifyCoordinate coordinate (i) (i) ]


piecesWithTakeMoves currentBoard itsFirstPlayersTurn =
    filter (\x -> takeMovesFromPosition currentBoard x itsFirstPlayersTurn /=[]) (allPiecesOfCurrentTurn currentBoard itsFirstPlayersTurn)

takeMovesFromPosition inBoard coordinate itsFirstPlayersTurn=
        filter (\x -> (elem x (emptyPositions inBoard)) &&
                    getPieceAtPosition inBoard (middleCoordinate x coordinate) /= Empty &&
                    isWhite (getPieceAtPosition inBoard (middleCoordinate x coordinate)) /= itsFirstPlayersTurn)
            (takePiecePossibleMoves (getPieceAtPosition inBoard coordinate) coordinate)
takePiecePossibleMoves :: Checkers -> Coordinate -> [Coordinate]
takePiecePossibleMoves piece coordinate = piecePossibleMoves piece coordinate 2

