module Main where
import IO
import Control.Monad

-- the woim data type:
data WoimItem a = WoimMultiLine [a] | WoimLine a | WoimOperator a | WoimState a deriving (Eq, Show, Read)
data Woim a = Woim a [Woim a] deriving (Eq, Show, Read)

-- Splits a woim line in to (indentation, text)
split n ('\t':xs) = split (8 + n) xs
split n (' ':xs)  = split (1 + n) xs
split n xs        = (n, xs)

splitAll z = [ split 0 y | y <- z ]

-- Checks if indentation level = +2
ismLine n [] = False
ismLine n ((na, xs):ys) = na == n + 2

parseLines :: [(Integer, [a])] -> [(Integer, WoimItem [a])]
parseLines [] = []
parseLines ((n, xs):ys) 
     | ismLine n ys = let (accumulated, rest) = parseMultiLines (n + 2) ys
                      in  (n, WoimMultiLine  (xs:accumulated)) : parseLines rest
     | otherwise    = (n, WoimLine xs) : parseLines ys


-- When multiline occurs, collect all lines on that level.
parseMultiLines :: Integer -> [(Integer, [a])] -> ([[a]], [(Integer, [a])])
parseMultiLines n [] = ([],[])
parseMultiLines n t@((nx, xs):ys) 
    | nx == n   = let (txt, rest) = parseMultiLines nx ys
                  in ((xs:txt), rest)
    | otherwise = ([], t)

type Token a = [(Integer, a)]
-- thanx to |Jedai| on irc.freenode.org 
-- for helping, debuging and cleaning up this one
-- Nice working with you :)
buildTree :: Integer -> Token a -> (Token a, [Woim a])
buildTree _ [] = ([],[])
buildTree n xxs@((level,x):xs)
    | level < n  = (xxs, [])
    | otherwise  = let (besides, children) = buildTree (level+8) xs        -- do we have children? if no level < n
                       (uncles, siblings)  = buildTree level besides       -- get my (level,x) brothers
                   in (uncles, Woim x children : siblings)

printTabs n = putStr $ replicate n '\t'

printTree :: Int -> [Woim String] -> IO()
printTree n [] = putStr ""
printTree n ((Woim txt sub):rest) = do 
        printTabs n
        putStrLn txt
        printTree (n + 1) sub
        printTree n rest

getTree :: (Token a, [Woim a]) -> [Woim a]
getTree (_,woim) = woim

main = do 
  woimLines <- lines `liftM` readFile "/home/knobo/prog/haskell/woim/woim.woim"
  let foo = parseLines $ splitAll woimLines in
      print $ buildTree (-1) foo
  
--  printTree 0 $ getTree $ buildTree (-1) $ parseLines $ splitAll woimLines
--  printTree 0 $ getTree $ buildTree (-1) $ multiLines $ splitAll woimLines

