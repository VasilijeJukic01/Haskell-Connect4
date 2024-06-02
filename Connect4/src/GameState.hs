module GameState (

)where

import Data.List (elemIndices)
import Data.List (tails)

-- Board [Row [ P, C, C, C, P], Row [ P, P, P, P, P], Row [ P, P, P, P, P], Row [ P, P, P, P, P], Row [ P, P, P, P, P]]

newtype Board a = Board [Row a] 
newtype Row a = Row [a] deriving Eq

data Field = C|Z|P deriving (Show, Eq, Enum)
data Move a = Move {player :: Player, field :: (Int, Int)} deriving Show
data Player = P1|P2 deriving (Show, Eq)

playerSymbol :: Player -> Field 
playerSymbol P1 = C
playerSymbol P2 = Z


instance Show a => Show (Row a) where
    show (Row a) = (foldl (\acc x -> acc ++ show x ++ "|") "|" a) ++ "\n"


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
addIndiciesToList connect4 = zip [0,1..] (map (zip[0,1..]) $ boardToList connect4)


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


chechHorizontal :: Board Field -> Player -> Bool
chechHorizontal (Board rows) p 
    | p == P1 = any checkDifferences [fieldsC row | row <- rows]
    | p == P2 = any checkDifferences [fieldsZ row | row <- rows]

        
-- endGame :: Board Field -> Player -> Int
-- endGame table player 



