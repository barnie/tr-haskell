import System.Environment
import System.IO (isEOF)

--args function
version = putStr "version 1.0\nauthor: Piotr Kruk\nemail: piotr@kruk.co\n"

-- function to help

ifExist x [] = []
ifExist x (f:fg) = if ( x == (fst f)) then [f] else ifExist x fg

translate _ [] = []
translate ds (f:fg) = if (null (ifExist f ds)) == True then [f] ++ (translate ds fg) else [ ( snd (head (ifExist f ds)) ) ] ++ (translate ds fg)

prepareTranslation aa bb to_translate = translate (zip (checkArgs ' ' aa) (checkArgs ' ' bb)) to_translate

helper ::(Show a, Eq a)=>[a] -> IO ()
helper initialState = do end <- isEOF
                         if end
                             then putStr ""
                             else do linein <- getLine
                                     let a = show (initialState!!0)
                                     let z = show (initialState !! 1)
                                     putStrLn (prepareTranslation a z linein )
                                     helper(initialState)


checkArgs x [] = []
checkArgs prev (f:fg) =
  if f == '-' then
    [prev..head(fg)] ++ checkArgs (head fg) (drop 1 fg)
  else if length(fg) > 0 && (head fg) /= '-' then
     [f] ++ checkArgs f fg
  else if (length fg) == 0 then
    [f]
  else
    checkArgs f fg

main = do
    args <- getArgs
    if args == ["-v"] then version
    else if length(args) == 2 then (helper args)
    else print $ args


myLoop = do done <- isEOF
            if done
              then putStrLn "Bye!"
              else do inp <- getLine
                      putStrLn (inp)
                      myLoop
