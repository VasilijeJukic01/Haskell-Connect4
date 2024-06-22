module GameState (
    applyValidMoves,
    emptyFields,
    listToBoard,
    endGame,
    Board,
    Player (P1, P2),
    Field (C, Z, P),
)where

import Data.List ( elemIndices, tails, transpose )

-- Board [Row [ Z, C, Z, P, P], Row [ C, P, Z, C, P], Row [ C, P, C, P, Z], Row [ C, P, Z, P, C], Row [ P, Z, C, Z, P]]

newtype Board a = Board [Row a]
newtype Row a = Row [a] deriving Eq

data Field = C|Z|P deriving (Show, Eq, Enum)
data Move a = Move {player :: Player, field :: (Int, Int)} deriving Show
data Player = P1|P2 deriving (Show, Eq)


playerSymbol :: Player -> Field
playerSymbol P1 = C
playerSymbol P2 = Z


instance Show a => Show (Row a) where
    show (Row a) = foldl (\acc x -> acc ++ show x ++ "|") "|" a ++ "\n"


instance Show a => Show (Board a) where
    show (Board a) = concatMap show a


rowToList :: Row a -> [a]
rowToList (Row fields) = fields


listToRow :: [a] -> Row a
listToRow = Row


boardToList :: Board a -> [[a]]
boardToList (Board rows) = map rowToList rows


listToBoard :: [[a]] -> Board a
listToBoard rows = Board (map Row rows)


addIndiciesToList :: Board Field -> [(Int, [(Int, Field)])]
addIndiciesToList connect4 = zip [0,1..] (map (zip [0,1..]) $ boardToList connect4)


emptyFieldsRow :: Row Field -> [Int]
emptyFieldsRow (Row list) = elemIndices P list


emptyFields :: Board Field -> [(Int, Int)]
emptyFields (Board rows) = do
    (rowIndex, row) <- zip [0..] rows -- rowIndex indeks reda, a row je sadržaj tog reda.
    colIndex <- emptyFieldsRow row -- iterira kroz kolone u trenutnom redu i gleda gde je P
    return (rowIndex, colIndex) -- kreira par koji je P za index


validMoves :: Player -> Board Field -> [Move Field]
validMoves player connect4 = map (\f -> Move {player = player, field = f}) (emptyFields connect4) -- dobija listu slobodnih polja i za tog igraca mu prikazuje na koja polja moze da odigra


changeOnIndices :: (Int, Int) -> Field -> [(Int, [(Int, Field)])] -> [[Field]]
changeOnIndices (x,y) field = map (\(x1, row) -> if x1==x then
    map (\(y2, odlField) -> if y2==y then field else odlField) row else map snd row)


applyValidMoves :: Board Field -> Player -> (Int, Int) -> Board Field
applyValidMoves table player coordinates = listToBoard (changeOnIndices coordinates  (playerSymbol player) (addIndiciesToList table))


fieldsC :: Row Field -> [Int]
fieldsC (Row list) = elemIndices C list

fieldsZ :: Row Field -> [Int]
fieldsZ (Row list) = elemIndices Z list

checkDifferences :: [Int] -> Bool
checkDifferences xs = checkDifferences' xs 0

checkDifferences' :: [Int] -> Int -> Bool
checkDifferences' _ 3 = True
checkDifferences' [] _ = False
checkDifferences' [_] _ = False
checkDifferences' (x:y:xs) count
    | abs (x - y) == 1 = checkDifferences' (y:xs) (count + 1)
    | otherwise = checkDifferences' (y:xs) 0


chechHorizontal :: Board Field -> Player -> Bool -- ali i za vertikal kad se odradi transponovanje
chechHorizontal (Board rows) p
    | p == P1 = any checkDifferences [fieldsC row | row <- rows]
    | p == P2 = any checkDifferences [fieldsZ row | row <- rows]


transposeBoard :: Board a -> Board a
transposeBoard (Board rows) = listToBoard (transpose (boardToList (Board rows)))


{-
all mora da mathc - uje sva polja da bi bila ista, pravi listu od 0 do 3 kako bi video 
da li su 4 elementa na dijagonali

indicies - pravi samo listu parova koordinata
-}

checkDiagonal :: Board Field -> Player -> Bool
checkDiagonal board player = any (checkDiagonalDirection 1 1) indices || any (checkDiagonalDirection 1 (-1)) indices
  where
    symbol = playerSymbol player -- uzima C ili Z
    boardList = boardToList board
    rowCount = length boardList -- duzina redova, granica mreze 
    colCount = length (head boardList) -- duzina kolona
    indices = [(r, c) | r <- [0..rowCount-1], c <- [0..colCount-1]]

    checkDiagonalDirection :: Int -> Int -> (Int, Int) -> Bool
    checkDiagonalDirection dirRow dirCol (row, col) = all (\i -> getField (row + i * dirRow) (col + i * dirCol) == Just symbol) [0..3]
    getField r c
      | r >= 0 && r < rowCount && c >= 0 && c < colCount = Just (boardList !! r !! c) -- proverava granice mreze, i ukoliko je uspeo vrati sta se nalazi na tom polju P ili C
      | otherwise = Nothing


endGame :: Board Field -> Player -> Bool
endGame table player = chechHorizontal table player ||
                       chechHorizontal (transposeBoard table) player ||
                       checkDiagonal table player



----------------------------------------------------------------------------------
-- Treca tacka

-- Tip podataka za stanje igre
data BoardState a = BoardState {
    board :: Board a,
    playerToMove :: Player
} deriving Show

-- Tip podataka za validno i nevalidno stanje
data GameState a = Valid a | Invalid String deriving Show

-- Definišemo operacije za manipulaciju stanjem igre
newtype GameStateOp s a = GameStateOp {
    run :: BoardState s -> (GameState a, BoardState s)
} 

-- Funktor instanca za GameState
instance Functor GameState where
    fmap f (Valid a) = Valid (f a)
    fmap f (Invalid msg) = Invalid msg

instance Functor (GameStateOp currState) where
    fmap f (GameStateOp g) = GameStateOp $ \s ->
        let (result, newState) = g s
        in (fmap f result, newState)

instance Applicative (GameStateOp currState) where
    pure x = GameStateOp $ \s -> (Valid x, s)

    (GameStateOp rf) <*> (GameStateOp ra) = GameStateOp $ \s ->
        let (resultF, newState) = rf s
            (resultA, newState1) = ra newState
        in case (resultF, resultA) of
            (Valid f, Valid a) -> (Valid (f a), newState1)
            (Invalid msg, _) -> (Invalid msg, newState1)
            (_, Invalid msg) -> (Invalid msg, newState1)

instance Monad (GameStateOp currState) where
    return = pure

    (GameStateOp ra) >>= f = GameStateOp $ \s ->
        let (resultA, newState) = ra s
        in case resultA of
            Valid a -> let (GameStateOp rb) = f a in rb newState
            Invalid msg -> (Invalid msg, newState)


applyMove :: (Int,Int) -> GameStateOp Field Bool
applyMove (x, y) = GameStateOp $ \s ->
    if not (isValidMove (board s) (playerToMove s) (x, y))
    then (Invalid "Invalid move attempted", s)
    else (Valid True, BoardState
            { board = applyValidMoves (board s) (playerToMove s) (x, y)
            , playerToMove = changePlayer (playerToMove s)
            })


isValidMove :: Board Field -> Player -> (Int, Int) -> Bool
isValidMove (Board rows) player (x, y)
    | x < 0 || x >= length rows = False  -- Provera da li je x van opsega
    | y < 0 || y >= length (unRow (rows !! x)) = False  -- Provera da li je y van opsega
    | getField (Board rows) (x, y) /= P = False  -- Provera da li je polje već popunjeno
    | otherwise = True

-- Pomoćna funkcija za dobijanje vrednosti polja na određenim koordinatama
getField :: Board Field -> (Int, Int) -> Field
getField (Board rows) (x, y) = unRow (rows !! x) !! y

-- Funkcija za konverziju Row u listu Field
unRow :: Row a -> [a]
unRow (Row xs) = xs

-- Funkcija za promenu igrača
changePlayer :: Player -> Player
changePlayer P1 = P2
changePlayer P2 = P1

-- Testiranje sekvence applyMoves
applyMoves :: GameStateOp Field Bool
applyMoves = do
    applyMove (1, 1)
    applyMove (1, 2)
    applyMove (1, 3)
    applyMove (0, 0)
    applyMove (0, 0)

runGameStateOp :: GameStateOp s a -> BoardState s -> (GameState a, BoardState s)
runGameStateOp (GameStateOp op) = op

main :: IO ()
main = do
  let initialBoardState =
        BoardState
          { board = listToBoard (replicate 6 (rowToList (listToRow (replicate 7 P))))
          , playerToMove = P1
          }
      (result, finalBoardState) = run (applyMoves) initialBoardState
  putStrLn "Final board state after applying moves:"
  print (board finalBoardState)
  putStrLn "Result:"
  print result


