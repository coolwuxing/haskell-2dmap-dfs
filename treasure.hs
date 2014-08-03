import System.IO
import Data.Char

markMap :: [[Char]] -> Int -> Int -> Char -> [[Char]]
markMap [] i j c = []
markMap strList i j c 
    | isOut = strList
    | (strList!!i!!j) == '-' || (strList!!i!!j) == '+' = let modified = take j (strList!!i) ++ ( c : drop (j + 1) (strList!!i))
                  in take i strList ++ (modified :drop (i + 1) strList)
    | otherwise = strList
    where isOut = i < 0 || j < 0 || (i >= length strList) || (j >= length (strList!!i))

deadEnd :: Int -> Int -> [String] -> Bool
deadEnd i j [] = True
deadEnd i j map
    | i < 0 || j < 0 = True
    | i >= length map = True
    | j >= length (map!!i) = True
    | (map!!i!!j) == '#' = True
    | (map!!i!!j) == '+' = True
    | (map!!i!!j) == '!' = True
    | otherwise = False

depthFirst :: Int -> Int -> Int -> ([String],Int) -> ([String],Int)
depthFirst i j _ ([],_) = ([],-1) 
depthFirst i j direction (map,rst)  
    | rst > 0 = (map,rst)
    | rst > -4 && isDead = (map, rst-1)
    | (map!!i!!j) == '@' = (map, 1)
    | rst == -4 = let failed_map = markMap map i j '!'
                  in (failed_map, -direction)
    | otherwise = depthFirst i j direction (depthFirst (i-1) j 4 (depthFirst i (j-1) 3 (depthFirst (i+1) j 2 (depthFirst i (j+1) 1 (marked_map,0)))))
    where marked_map = markMap map i j '+'
          isDead = deadEnd i j map

main = do
    strMap <- readFile "map.txt"
    putStr "This is my challenge:\n\n"
    let m = lines strMap
    mapM_ putStrLn m
    let (m',rst) = depthFirst 0 0 1 (m,0)
    if rst > 0
        then putStrLn "Woo hoo, I found the treasure :-)"
        else putStrLn "Uh oh, I could not find the treasure :-("
    mapM_ putStrLn m'

