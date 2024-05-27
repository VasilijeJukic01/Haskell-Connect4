module GameState (

)where



-- Board [Row [ P, P, P, P, P], Row [ P, P, P, P, P], Row [ P, P, P, P, P], Row [ P, P, P, P, P], Row [ P, P, P, P, P]]

{-
|   |   |   |   |  
|   |
|   |
|   |

-}

newtype Board a = Board [Row a] 
newtype Row a = Row [a] deriving Eq

data Field = C|Z|P deriving (Show, Eq, Enum)
data Move = Move {player :: Player, field :: (Int, Int)} deriving Show
data Player = P1|P2 deriving (Show, Eq)


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


addIndiciesToList :: Board Field -> [(Int, [(Int, Field)])]
addIndiciesToList connect4 = zip [0,1..] (map (zip[0,1..]) $ boardToList connect4)


validMoves :: Player -> Board Field -> [Move Field]
validMoves player connect4 - map (\table -> )




