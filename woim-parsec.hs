module Main where
import System
import Control.Monad
import Control.Monad.Writer
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

-- the woim data type:

data WoimItem = WoimMultiLine [String] Mlid  | 
                WoimLine String              | 
                WoimOperator String Data     | 
                WoimQualifier String Data    |
                WoimTag String               |
                WoimState String deriving (Eq, Show, Read)

data Woim = Woim WoimItem WoimList deriving (Eq, Show, Read)

type Data = String
type Mlid = String
type WoimList = [Woim]           -- Maybe this one is overkill?


parseWoimOperator :: Parser WoimItem
parseWoimOperator = do operator <- manyTill (noneOf " \n") (char ':')
                       odata    <- do  manyTill anyChar (char '\n')
                       return $ WoimOperator operator odata


---------------------------------------------------------------------------
-- parseQualifCount = Parser String.                                     --
-- parseQualifCount = do sign <- optional (oneOf "+-<>?")                --
--                       count <- optional (many1 digit)                 --
--                       range <- optional ((char ',') >> (many1 digit)) --
---------------------------------------------------------------------------
                      


parseWoimQualifier :: Parser WoimItem 
parseWoimQualifier = do qualif <- between (char '[') (char ']') (many1 (noneOf "[]"))
                        option [] (many1 space)
                        rest <- manyTill anyChar (char '\n')
                        return $ WoimQualifier qualif rest

--                                       --
-- parseWoimTag :: Parser WoimItem       --
-- parseWoimTag =                        --
--                                       --
-- parseWoimState :: Parser WoimItem     --
-- parseWoimState =                      --


parseWoimDescription :: Parser WoimItem
parseWoimDescription = do x <- manyTill anyChar (char '\n')
                          return $ WoimLine x 

parseWoimLine :: Parser WoimItem
parseWoimLine =  try parseWoimOperator <|> parseWoimQualifier <|> parseWoimDescription

parseWoimMultiLine :: Int -> Parser WoimItem
parseWoimMultiLine tabs = do id <- parseIdentifyer
                             lines <- (manyTill anyChar (char '\n')) `sepBy`  try (indent tabs >> (count (idMlineCountSpaces id) space))
                             return $ WoimMultiLine lines id

idMlineCountSpaces :: String -> Int
idMlineCountSpaces ('*':_) = 2
idMlineCountSpaces str = 1 + length str

parseIdentifyer :: Parser String
parseIdentifyer = try (digit `endBy1` char '.'  <|> string "* ")

parseWoimItem :: Int -> Parser Woim
parseWoimItem tabs = do try (indent tabs)
                        item <- do try (parseWoimMultiLine tabs) <|> parseWoimLine
                        subtree <- option []  . many1 . parseWoimItem $ tabs + 1
                        return $ Woim item subtree

indent :: Int -> Parser String
indent n = count n (char '\t')

main = do args <- getArgs
          out <-  parseFromFile (many1 (parseWoimItem 0)) (head args)
          case out of
            Left err -> putStrLn $ "No match in file: " ++ show err
            Right x ->  putStrLn $ "Found value:\n"  ++ show x


