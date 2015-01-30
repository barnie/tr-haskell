module Main (main) where
import System.Environment
import System.IO (isEOF)

version
  = putStr "version 1.0\nauthor: Piotr Kruk\nemail: piotr@kruk.co\n"

man
  = putStr "The same like linux man tr, except dictionaries i support only : alnum, alpha, upper,digit,space, \" \\n \" "

--check if element exist in list and return this element
ifExist x [] = []
ifExist x (f : fg) = if (x == (fst f)) then [f] else ifExist x fg


--check if element return in list and return True/False
if_exist [] _ = False
if_exist (f : fg) x = if (f == x) then True else if_exist fg x


--normal translation
translate _ [] = []
translate ds (f : fg)
  = if (null (ifExist f ds)) == True then [f] ++ (translate ds fg)
      else [(snd (head (ifExist f ds)))] ++ (translate ds fg)


--check args and zip them
prepareTranslation aa bb to_translate
  = translate (zip (checkArgs ' ' aa) (checkArgs ' ' bb))
      to_translate


--sometimes i get " from IO is good practise to remove them to avoid bugs
cutit [] = []
cutit (f : fg) = if f == '"' then cutit fg else [f] ++ cutit fg


--normal translation with truncate option
helper :: (Show a, Eq a) => [a] -> IO ()
helper initialState
  = do end <- isEOF
       if end then putStr "" else
         do linein <- getLine
            let a = (cutit (show (initialState !! 0)))
            let z = (cutit (show (initialState !! 1)))
            putStrLn ((prepareTranslation a z linein))
            helper (initialState)


--last element from list
listLast :: [a] -> a
listLast [x] = x
listLast (_ : xs) = listLast xs


--k element of last elements from list
complete :: (Eq b, Num b) => [a] -> b -> [a]
complete _ 0 = []
complete x y = [listLast x] ++ (complete x (y - 1))

splitAt' = \n -> \xs -> (take n xs, drop n xs)

multiple 0 _ = []
multiple n x = [x] ++ multiple (n-1) x

transform x = (snd (splitAt' (length (snd x)) (fst x)))

--Normal translation
transl ::  [String] -> IO ()
transl initialState
  = do end <- isEOF
       let a = (initialState !! 0)
       let z = (initialState !! 1)
       if end then putStr "" else
         do linein <- getLine
            putStrLn ((prepareTranslation a z linein))
            transl (initialState)


--check if args are correct [a-z] case
checkArgs x [] = []
checkArgs prev (f : fg)
  = if f == '-' then
      [prev .. head (fg)] ++ checkArgs (head fg) (drop 1 fg) else
      if length (fg) > 0 && (head fg) /= '-' then [f] ++ checkArgs f fg
        else if (length fg) == 0 then [f] else checkArgs f fg


--simple replace
replace :: (Eq a) => [a] -> [a] -> [a]
replace [] _ = []
replace (s : sg) find
  = if (if_exist find s) == True then replace sg find else
      [s] ++ replace sg find


-- delete -d options for tr
deleter :: [Char] -> IO ()
deleter initialState
  = do end <- isEOF
       if end then putStr "" else
         do linein <- getLine
            putStrLn (replace linein initialState)
            deleter (initialState)


--convert from array of string to String
convert_arr_string_to_string [] = []
convert_arr_string_to_string (f : fx)
  = (map (\ c -> c) f) ++ convert_arr_string_to_string fx


--simple replace
replaceS [] _ _ _ = []
replaceS (f : fg) x 1 prev
  = if (if_exist x f) == True then [f] ++ replaceS fg x 0 [f] else
      [f] ++ replaceS fg x 1 " "
replaceS (f : fg) x 0 prev
  = if (f == head prev) then replaceS fg x 0 [f] else
      if (if_exist x f) then [f] ++ replaceS fg x 0 [f] else
        [f] ++ replaceS fg x 1 " "


--remove duplicates
remove1 :: [String] -> IO ()
remove1 initialState
  = do end <- isEOF
       if end then putStr "" else
         do linein <- getLine
            let a = (cutit (show (initialState !! 0)))
            let result = (replaceS linein a 1 " ")
            if (length initialState) == 1 then
              putStrLn( result  )
            else
              putStrLn(replaceS (prepareTranslation a (initialState !! 1) linein) (initialState !! 1) 1 " " )
            remove1 (initialState)


--light delete
light_remove :: (Eq a) => [a] -> a -> [a] -> [a]
light_remove [] _ _ = []
light_remove (f : fg) replace s
  = if (if_exist s f) == True then [f] ++ light_remove fg replace s
      else [replace] ++ light_remove fg replace s


--complete -c flag
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


--check dictionaries
checkDic "[:alnum:]" = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
checkDic "[:alpha:]" = ['a' .. 'z'] ++ ['A' .. 'Z']
checkDic "[:digit:]" = ['0' .. '9']
checkDic "[:lower:]" = ['a' .. 'z']
checkDic "[:upper:]" = ['A' .. 'Z']
checkDic "[:space:]" = " "
checkDic "\\n" = "\n"
checkDic "\n" = "\n"
checkDic "[0-9]" = ['0' .. '9']
checkDic "[a-z]" = ['a'..'z']
checkDic "[A-Z]" = ['A' .. 'Z']
checkDic x = x

compare1::(String, String)->String
compare1 x = if length(fst x) > length(snd x) then (snd x) ++ (snd (splitAt' (length (snd x)) (fst x)) ) else snd x

prepare ::  [String] -> [String]
prepare args =
  [checkDic (args !! 0), checkDic(args !! 1) ++ (multiple ( ( length (checkDic (args !! 0)) ) - ( length (checkDic (args !! 1)) ) ) (last(checkDic (args !! 1)) ))]

prepare1 ::[String] -> [String]
prepare1 args = if (length args) > 1 then prepare args else [checkDic(args !! 0)]

main
  = do args <- getArgs
       if args == ["-v"] then version else
        if args == ["-h"] then man else
         if head (args) == "-t" then (helper (prepare(tail args))) else
           if head (args) == "-d" && length(args) == 2 then
             deleter (checkDic(convert_arr_string_to_string (tail args)))else
             if head (args) == "-s" then remove1 (prepare1(tail args)) else
               if head (args) == "-c" || head (args) == "-C" then
                 complet ([checkDic(args !! 1), args !! 2], "") else
                 if length (args) == 2 then
                  transl ( prepare(args) )
                   else man
