module Main where
import IO
import Control.Monad

-- the woim data type:
data Woim a = Woim a [Woim a] deriving (Eq, Show, Read)

-- Splits a woim line in to (indentation, text)
split n ('\t':xs) = split (8 + n) xs
split n (' ':xs)  = split (1 + n) xs
split n xs        = (n, xs)

splitAll z = [ split 0 y | y <- z ]

-- For multi lines, I have changed the rules to be 
-- Next line (line n) is indented with two spaces, then multiline
-- until indentation is not eqlual to (line n) + two spaces any more.
multiLines :: [(Integer, [a])] -> [(Integer,[a])]
multiLines [] = []
multiLines ((n, xs):ys) 
     | ismLine n ys = let (accumulated, rest) = collectMulti (n + 2) ys
                      in  (n,(xs ++ accumulated)) : multiLines rest
     | otherwise    = (n,xs) : multiLines ys

-- Checks if indentation level = +2
ismLine n [] = False
ismLine n ((na, xs):ys) = na == n + 2

-- When multiline occurs, collect all lines on that level.
collectMulti :: Integer -> [(Integer, [a])] -> ([a], [(Integer, [a])])
collectMulti n [] = ([],[])
collectMulti n t@((nx, xs):ys) 
    | nx == n   = let (txt, rest) = collectMulti nx ys
                  in (xs ++ txt, rest)
    | otherwise = ([], t)


type Token a = [(Integer,a)]
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
  printTree 0 $ getTree $ buildTree (-1) $ multiLines $ splitAll woimLines


