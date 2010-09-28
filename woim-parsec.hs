module Main where
import System
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

-- the woim data type:
data WoimItem = WoimMultiLine [String] | WoimLine String | WoimOperator String | WoimState String deriving (Eq, Show, Read)
data Woim = Woim WoimItem WoimList deriving (Eq, Show, Read)

type WoimList = [Woim]           -- Maybe this one is overkill?
type Token a = (Integer, a)
type Tokens a = [Token a]

parseWoimLine :: Parser WoimItem
parseWoimLine = do x <- manyTill anyChar (char '\n')
                   return $ WoimLine x 
  
parseWoimMultiLine :: Int -> Parser WoimItem
parseWoimMultiLine tabs = do id <- parseIdentifyer
                             lines <- manyTill anyChar (char '\n') `sepBy` try (indent tabs  >> count (idMlineCountSpaces id) space)
                             return $ WoimMultiLine lines

idMlineCountSpaces :: String -> Int
idMlineCountSpaces ('*':_) = 0
idMlineCountSpaces str = 1 + length str

parseIdentifyer :: Parser String
parseIdentifyer = digit `endBy1` char '.'  <|> string "* "
                             
parseWoimItem :: Int -> Parser Woim
parseWoimItem tabs = do try (indent tabs)
                        item <- parseWoimMultiLine tabs <|> parseWoimLine
                        subtree <- option [] (try (many1 (parseWoimItem (tabs + 1))))
                        return $ Woim item subtree
                       
indent :: Int -> Parser String
indent n = count n (char '\t')

main = do args <- getArgs
          out <-  parseFromFile (many (parseWoimItem 0)) (head args)
          case out of
            Left err -> putStrLn $ "No match in file: " ++ show err
            Right x ->  putStrLn $ "Found value:\n"  ++ show x



