import System.Environment


--args function
version = putStr "version 1.0\nauthor: Piotr Kruk\nemail: piotr@kruk.co\n" 

-- function to help

ifExist x [] = []
ifExist x (f:fg) = if ( x == (fst f)) then [f] else ifExist x fg

translate _ [] = []
translate ds (f:fg) = if (null (ifExist f ds)) == True then [f] ++ (translate ds fg) else [ ( snd (head (ifExist f ds)) ) ] ++ (translate ds fg)

prepareTranslation aa bb to_translate = translate (zip aa bb) to_translate


main = do
    args <- getArgs
    if args == ["-v"] then version
    else if length(args) == 2 then print (prepareTranslation (head args) (args !! 1) "aabbzzabab")
    else print $ args
