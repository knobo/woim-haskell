module Main where
import IO
import Control.Monad

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
parseLines ((n, xs):ys) 
  | ismLine n ys = let (accumulated, rest) = parseMultiLines (n + 2) ys
                   in  (n, WoimMultiLine  (xs:accumulated)) : parseLines rest
  | otherwise    = (n, WoimLine xs) : parseLines ys

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
printMultiLineList n (x:xs) = 
  do printTabs n
     putStr "  "
     putStrLn x
     printMultiLineList n xs

getTree :: (Tokens WoimItem, WoimList) -> WoimList
getTree = snd

main = do 
  woimLines <- lines `liftM` readFile "/home/knobo/prog/haskell/woim/woim.woim"
  let tokens = parseLines $ splitAll woimLines 
    in printDot $ getTree $ buildTree (-1) tokens
 -- printTree 0 $ getTree $ buildTree (-1) tokens

printBackPointer :: String -> IO()
printBackPointer [] = return ()
printBackPointer (x:[]) = return ()
printBackPointer (x:xs) = 
  do mapM_ putStr [xs, " -> ", (x:xs), ";\n" ]

labelFormat (WoimMultiLine []) = []
labelFormat (WoimMultiLine (x:xs)) = do 
  quoteLabel x ++ labelFormat (WoimMultiLine xs)
labelFormat (WoimLine x) = do 
  quoteLabel x

quoteLabel :: String -> String
quoteLabel [] = []
quoteLabel (x:xs) = do
  (case x of
    '\"'      ->  "\\\""
    '\\'      ->  "\\"
    '\n'      ->  "\\n"
    otherwise ->  [x])  ++  quoteLabel xs

printDotItems :: String -> WoimList -> IO()
printDotItems _ [] = return ()
printDotItems id@(i:is) woimlist@((Woim x children):siblings) = 
  do mapM_ putStr [id, " [label=\"", labelFormat x, "\"];\n"]
     printBackPointer id
     head [ printDotItems (newid:id) children | newid <- ['a'..] ]
     printDotItems ((succ i):is) siblings

printDot :: WoimList -> IO()
printDot woim = do
  putStrLn "digraph G {"
  head [ printDotItems [id] woim | id <- ['a'..] ]
  putStrLn "}"

withFile :: FilePath -> IOMode -> ( Handle -> IO a ) -> IO a
withFile path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

