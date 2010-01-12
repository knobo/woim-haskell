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

toList :: a -> WoimList -> (WoimItem -> a -> Writer String ()) -> (a -> a) -> (a -> a) -> Writer String ()
toList _ [] _ _ _ = return ()
toList id ((Woim item children):siblings) format childrenId siblingId = do 
  format item id 
  toList (childrenId id) children format childrenId siblingId
  toList (siblingId id) siblings format childrenId siblingId

-- ------------------------------- --
-- Format dots......  for GraphViz --
-- ------------------------------- --
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

formatDot :: WoimItem -> String -> Writer String ()
formatDot item id = do
  tell (id ++ " [label=\"" ++ labelFormat item ++ "\"];\n")
  tell $ backPointer id
  return ()
  
chNextId :: String -> String
chNextId id = ('a':id)

siNextId :: String -> String 
siNextId id@(i:is) = ((succ i):is)

dot :: WoimList -> Writer String ()
dot woim = do
  tell "digraph G {\n"
  toList "a" woim formatDot chNextId siNextId
  tell "}\n"
  return ()

-- ----------------------------------- --
-- End of formating dots. for GraphViz --
-- ----------------------------------- --

flatten :: WoimList -> Writer String ()
flatten tree = do 
  toList 0 tree 
    (\x y -> do tell ((replicate y '\t') ++  (labelFormat x) ++ "\n"); return ()) -- This is not done..
    succ id
  return ()
         
parseFile :: (WoimList -> Writer String ()) -> IO ()
parseFile p = do   
  woimLines <- lines `liftM` readFile "/home/knobo/prog/haskell/woim/woim.woim"
  let tokens   = parseLines $ splitAll woimLines 
  let (_,tree) = buildTree (-1) tokens
  let (_,out)  = runWriter $ p tree
    in putStr out
       
main :: IO ()       
main = do 
  parseFile dot 

withFile :: FilePath -> IOMode -> ( Handle -> IO a ) -> IO a
withFile path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result
