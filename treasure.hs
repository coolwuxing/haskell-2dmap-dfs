import System.IO
import Data.Char

markMap :: [[Char]] -> Int -> Int -> Char -> [[Char]]
markMap [] i j c = []
markMap strList i j c = let modified = take j (strList!!i) ++ ( c : drop (j + 1) (strList!!i))
                        in take i strList ++ (modified :drop (i + 1) strList)

deadEnd :: Int -> Int -> [String] -> Bool
deadEnd i j [] = True
deadEnd i j map
    | i < 0 || j < 0 = True
    | i >= length map = True
    | j >= length (map!!i) = True
    | (map!!i!!j) == '#' = True
    | (map!!i!!j) == '+' = True
    | otherwise = False

depthFirst :: Int -> Int -> ([String],Int) -> ([String],Int)
depthFirst i j ([],_) = ([],-1) 
depthFirst i j (map,rst)  
    | rst > 0 = (map,rst)
    | (map!!i!!j) == '@' = (map, 1)
    | rst > -4 && isDead = (map, rst-1)
    | rst == -4 = let failed_map = markMap map i j '!'
                  in (failed_map, -1)
    | otherwise = depthFirst i j (depthFirst (i-1) j (depthFirst i (j-1) (depthFirst (i+1) j (depthFirst i (j+1) (marked_map,0)))))
    where marked_map = markMap map i j '+'
          isDead = deadEnd i j map

--depthFirst i j map  
--    | snd rightmove == 1 = rightmove
--    | snd down == 1 = down
--    | snd left == 1 = left
--    | snd up == 1 = up
--    | otherwise = let failed_map = markMap map i j '!'
--                  in (failed_map, -1)
--    where marked_map = markMap map i j '+'
--          rightmove = (["sdfsdf","sdfsdf"],1)
--          down= (["sdfsdf","sdfsdf"],1)
--          left= (["sdfsdf","sdfsdf"],1)
--          up= (["sdfsdf","sdfsdf"],1)
--          rightmove = depthFirst i j+1 marked_map 
--          down = depthFirst i+1 j fst rightmove
--          left = depthFirst i j-1 fst down
--          up = depthFirst i-1 j fst left

main = do
    strMap <- readFile "map.txt"
    putStr "This is my challenge:\n\n"
    let m = lines strMap
--    print m
    mapM_ putStrLn m
    let (m',rst) = depthFirst 0 0 (m,0)
    if rst > 0
        then putStrLn "Woo hoo, I found the treasure :-)"
        else putStrLn "Uh oh, I could not find the treasure :-("
    mapM_ putStrLn m'

