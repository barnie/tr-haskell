module Main (main) where
import System.Environment
import System.IO (isEOF)

version
  = putStr "version 1.0\nauthor: Piotr Kruk\nemail: piotr@kruk.co\n"

ifExist x [] = []
ifExist x (f : fg) = if (x == (fst f)) then [f] else ifExist x fg

if_exist [] _ = False
if_exist (f : fg) x = if (f == x) then True else if_exist fg x

translate _ [] = []
translate ds (f : fg)
  = if (null (ifExist f ds)) == True then [f] ++ (translate ds fg)
      else [(snd (head (ifExist f ds)))] ++ (translate ds fg)

prepareTranslation aa bb to_translate
  = translate (zip (checkArgs ' ' aa) (checkArgs ' ' bb))
      to_translate

cutit [] = []
cutit (f : fg) = if f == '"' then cutit fg else [f] ++ cutit fg

helper :: (Show a, Eq a) => [a] -> IO ()
helper initialState
  = do end <- isEOF
       if end then putStr "" else
         do linein <- getLine
            let a = (cutit (show (initialState !! 0)))
            let z = (cutit (show (initialState !! 1)))
            putStrLn ((prepareTranslation a z linein))
            helper (initialState)

listLast :: [a] -> a
listLast [x] = x
listLast (_ : xs) = listLast xs

complete :: (Eq b, Num b) => [a] -> b -> [a]
complete _ 0 = []
complete x y = [listLast x] ++ (complete x (y - 1))

transl :: (Show a, Eq a) => [a] -> IO ()
transl initialState
  = do end <- isEOF
       let a = checkDic (cutit (show (initialState !! 0)))
       let z = checkDic (cutit (show (initialState !! 1)))
       if end then putStr "" else
         do linein <- getLine
            putStrLn (show (initialState !! 0))
            if (length (a) <= length (z)) then
              putStrLn ((prepareTranslation a z linein)) else
              putStrLn
                ((prepareTranslation (a ++ (complete a (length (z) - length (a))))
                    z
                    linein))
            transl (initialState)

checkArgs x [] = []
checkArgs prev (f : fg)
  = if f == '-' then
      [prev .. head (fg)] ++ checkArgs (head fg) (drop 1 fg) else
      if length (fg) > 0 && (head fg) /= '-' then [f] ++ checkArgs f fg
        else if (length fg) == 0 then [f] else checkArgs f fg

replace :: (Eq a) => [a] -> [a] -> [a]
replace [] _ = []
replace (s : sg) find
  = if (if_exist find s) == True then replace sg find else
      [s] ++ replace sg find

deleter :: [Char] -> IO ()
deleter initialState
  = do end <- isEOF
       if end then putStr "" else
         do linein <- getLine
            putStrLn (replace linein initialState)
            deleter (initialState)

convert_arr_string_to_string [] = []
convert_arr_string_to_string (f : fx)
  = (map (\ c -> c) f) ++ convert_arr_string_to_string fx

replaceS [] _ _ _ = []
replaceS (f : fg) x 1 prev
  = if (if_exist x f) == True then [f] ++ replaceS fg x 0 [f] else
      [f] ++ replaceS fg x 1 " "
replaceS (f : fg) x 0 prev
  = if (f == head prev) then replaceS fg x 0 [f] else
      if (if_exist x f) then [f] ++ replaceS fg x 0 [f] else
        [f] ++ replaceS fg x 1 " "

remove1 :: [String] -> IO ()
remove1 initialState
  = do end <- isEOF
       if end then putStr "" else
         do linein <- getLine
            let a = (cutit (show (initialState !! 0)))
            let result = (replaceS linein a 1 " ")
            if (tail initialState) == [] then
              putStrLn( result)
            else
              putStrLn((prepareTranslation a  (cutit (show (initialState !! 1))) result))
            remove1 (initialState)

light_remove :: (Eq a) => [a] -> a -> [a] -> [a]
light_remove [] _ _ = []
light_remove (f : fg) replace s
  = if (if_exist s f) == True then [f] ++ light_remove fg replace s
      else [replace] ++ light_remove fg replace s

complet :: ([String], [Char]) -> IO ()
complet initialState
  = do end <- isEOF
       let args = fst (initialState)
       let dictionary = head (args)
       let replace = (listLast (args !! 1))
       if end then putStr ((snd initialState) ++ [replace]) else
         do linein <- getLine
            let result = light_remove linein replace dictionary
            let z = (snd initialState) ++ result
            complet (args, z)


checkDic "[:alnum:]" = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
checkDic "[:alpha:]" = ['a' .. 'z'] ++ ['A' .. 'Z']
checkDic "[:digit:]" = ['0' .. '9']
checkDic "[:lower:]" = ['a' .. 'z']
checkDic "[:upper:]" = ['A' .. 'Z']
checkDic x = x


main
  = do args <- getArgs
       if args == ["-v"] then version else
         if head (args) == "-t" then (helper (tail args)) else
           if head (args) == "-d" then
             deleter (convert_arr_string_to_string (tail args)) else
             if head (args) == "-s" then remove1 (tail args) else
               if head (args) == "-c" || head (args) == "-C" then
                 complet ([args !! 1, args !! 2], "") else
                 if length (args) == 2 then (transl args) else print $ args
