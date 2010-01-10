module Main where
import IO
import Control.Monad
import Control.Monad.Writer

-- the woim data type:
data WoimItem = WoimMultiLine [String] | WoimLine String | WoimOperator String | WoimState String deriving (Eq, Show, Read)
data Woim = Woim WoimItem WoimList deriving (Eq, Show, Read)

type WoimList = [Woim]           -- Maybe this one is overkill?
type Token a = (Integer, a)
type Tokens a = [Token a]

-- Splits a woim line in to (indentation, text)
split :: Integer -> String -> Token String
split n ('\t':xs) = split (8 + n) xs
split n (' ':xs)  = split (1 + n) xs
split n xs        = (n, xs)

splitAll :: [String] -> Tokens String
splitAll z = [ split 0 y | y <- z ]

-- Checks if indentation level = +2, which gives multi line item
ismLine :: Integer -> Tokens a -> Bool
ismLine n [] = False
ismLine n ((na, xs):ys) = na == n + 2

parseLines :: Tokens String -> Tokens WoimItem
parseLines [] = []
parseLines ((n, string):ys) 
  | ismLine n ys = let (accumulated, rest) = parseMultiLines (n + 2) ys
                   in  (n, WoimMultiLine  (string:accumulated)) : parseLines rest
  | otherwise    = (n, WoimLine string) : parseLines ys

-- When multiline occurs, collect all lines on that level.
parseMultiLines :: Integer -> Tokens String -> ([String], Tokens String)
parseMultiLines n [] = ([],[])
parseMultiLines n t@((nx, xs):ys) 
  | nx == n   = let (txt, rest) = parseMultiLines nx ys
                in ((xs:txt), rest)
  | otherwise = ([], t)

buildTree :: Integer -> Tokens WoimItem -> (Tokens WoimItem, WoimList)
buildTree _ [] = ([],[])
buildTree n xxs@((level,x):xs)
  | level < n  = (xxs, [])
  | otherwise  = let (besides, children) = buildTree (level+8) xs       -- do we have children? if no level < n
                     (uncles, siblings)  = buildTree level besides      -- get my (level,x) brothers
                 in (uncles, Woim x children : siblings)

printTabs n = putStr $ replicate n '\t'

printTree :: Int -> WoimList -> IO()
printTree n [] = return ()
printTree n ((Woim (WoimLine txt) children):siblings) = do 
  printTabs n
  putStrLn txt
  printTree (n + 1) children
  printTree n siblings
printTree n ((Woim (WoimMultiLine (x:xs)) children):siblings) = do 
  printTabs n
  putStrLn x                             -- First line as normal
  printMultiLineList n xs                -- Rest with +2 spaces
  printTree (n + 1) children
  printTree n siblings

printMultiLineList :: Int -> [String] -> IO()
printMultiLineList n [] = return ()
printMultiLineList n (x:xs) = do 
  printTabs n
  putStr "  "
  putStrLn x
  printMultiLineList n xs

getTree :: (Tokens WoimItem, WoimList) -> WoimList
getTree = snd

labelFormat (WoimMultiLine [])     = []
labelFormat (WoimMultiLine (x:[])) = quoteLabel x 
labelFormat (WoimMultiLine (x:xs)) = quoteLabel x ++ "\\n" ++ labelFormat (WoimMultiLine xs)
labelFormat (WoimLine x)           = quoteLabel x

quoteLabel :: String -> String
quoteLabel [] = []
quoteLabel string = do x <- string
                       (case x of
                           '\"'      ->  "\\\""
                           '\\'      ->  "\\"
                           '\n'      ->  "\\n"
                           otherwise ->  [x])  

backPointer :: String -> String
backPointer []     = ""
backPointer (x:[]) = ""
backPointer (x:xs) = xs ++ " -> " ++ (x:xs) ++ ";\n" 

dotItems :: String -> WoimList -> Writer String String
dotItems _ [] = return ""
dotItems id@(i:is) ((Woim item children):siblings) = do 
  tell (id ++ " [label=\"" ++ labelFormat item ++ "\"];\n")
  tell $ backPointer id
  dotItems ('a':id) children
  dotItems ((succ i):is) siblings

dot :: WoimList -> Writer String String
dot woim = do
  tell "digraph G {\n"
  dotItems "a" woim
  tell "}\n"
  return ""

main = do 
  woimLines <- lines `liftM` readFile "/home/knobo/prog/haskell/woim/woim.woim"
  let tokens = parseLines $ splitAll woimLines 
  let (_,out) = runWriter $ dot $ getTree $ buildTree (-1) tokens
      in putStr out 

withFile :: FilePath -> IOMode -> ( Handle -> IO a ) -> IO a
withFile path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result
