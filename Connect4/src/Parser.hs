import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (digit, char, oneOf, endOfLine, space, spaces, anyChar, string)
import Text.Parsec.Combinator (many1, sepBy1, sepEndBy1, between)
import Control.Monad (void)
import GameState

parseField :: Parser Field
parseField = do
    field <- oneOf "CZ "
    case field of
        'C' -> return C
        'Z' -> return Z
        ' ' -> return P
        _   -> fail "Invalid field character"

parseRow :: Parser (Row Field)
parseRow = do
    char '|'
    fields <- sepBy1 parseField (char '|')
    string "\n"
    return $ Row fields

parseBoard :: Parser (Board Field)
parseBoard = do
    rows <- many1 parseRow
    let boardList = map rowToList rows
    return $ listToBoard boardList

parseCoordinate :: Parser (Int, Int)
parseCoordinate = do
    x <- many1 digit
    char ','
    y <- many1 digit
    return (read x - 1, read y - 1)

parseMoves :: Parser [(Int, Int)]
parseMoves = sepBy1 parseCoordinate endOfLine

parseGameState :: Parser (Board Field, [(Int, Int)])
parseGameState = do
    board <- parseBoard
    void $ many endOfLine
    moves <- parseMoves
    return (board, moves)

parseGameFile :: FilePath -> IO (Either ParseError (Board Field, [(Int, Int)]))
parseGameFile filePath = parseFromFile parseGameState filePath

applyParsedMoves :: FilePath -> IO ()
applyParsedMoves filePath = do
    parseResult <- parseGameFile filePath
    case parseResult of
        Left err -> print err
        Right (initialBoard, moves) -> do
            let initialBoardState = BoardState { board = initialBoard, playerToMove = P1 }
                (result, finalBoardState) = run (applyMovesFromList moves) initialBoardState
            putStrLn "Final board state after applying moves:"
            print (board finalBoardState)
            putStrLn "Result:"
            print result

main :: IO ()
main = do
    applyParsedMoves "fileForParser.txt" 
