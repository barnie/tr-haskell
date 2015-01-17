import System.Environment
import System.IO (isEOF)

--args function
version = putStr "version 1.0\nauthor: Piotr Kruk\nemail: piotr@kruk.co\n"

-- if exist in list

ifExist x [] = []
ifExist x (f:fg) = if ( x == (fst f)) then [f] else ifExist x fg

translate _ [] = []
translate ds (f:fg) = if (null (ifExist f ds)) == True then [f] ++ (translate ds fg) else [ ( snd (head (ifExist f ds)) ) ] ++ (translate ds fg)

prepareTranslation aa bb to_translate = translate (zip (checkArgs ' ' aa) (checkArgs ' ' bb)) to_translate

--there is a problem with conversion to string with show (sometimes i get ''abc or abc")
cutit [] = []
cutit (f:fg) = if f == '"' then cutit fg else [f] ++ cutit fg

-- -t (truncate option)

helper ::(Show a, Eq a)=>[a] -> IO ()
helper initialState = do end <- isEOF
                         if end
                             then putStr ""
                             else do linein <- getLine
                                     let a =checkDic ( cutit (show (initialState!! 0)) )
                                     let z =checkDic ( cutit (show (initialState !! 1)) )
                                     putStrLn ((prepareTranslation a z linein) )
                                     helper(initialState)
--get last element from list
listLast::[a]->a
listLast [x] = x
listLast(_:xs) = listLast xs
--return list of N last elements
complete::(Eq b, Num b)=>[a]->b->[a]
complete _ 0 = []
complete x y = [ listLast x ] ++ (complete x (y - 1))

--normal translation
transl ::(Show a, Eq a)=>[a] -> IO ()
transl initialState = do end <- isEOF
                         let a = checkDic ( cutit (show (initialState!! 0)) )
                         let z = checkDic ( cutit (show (initialState !! 1)) )
                         if end
                             then putStr ""
                             else do linein <- getLine
                                     putStrLn (show(initialState !! 0))
                                     if (length(a) <= length(z)) then
                                      putStrLn ((prepareTranslation a z linein) )
                                     else
                                      putStrLn ((prepareTranslation ( a ++ ( complete a (length(z) - length(a)) ) ) z linein) )
                                     transl(initialState)

--chech arguments for dictionary
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

--replace
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)


--remove all characters
deleter::[Char] -> IO ()
deleter initialState = do end <- isEOF
                          if end
                             then putStr ""
                             else do linein <- getLine
                                     putStrLn (replace linein initialState "" )
                                     deleter(initialState)

--convert string to [array]
convert_arr_string_to_string [] = []
convert_arr_string_to_string (f:fx) = (map (\c -> c) f) ++ convert_arr_string_to_string fx

--remove duplicates
replaceS [] x counter = []
replaceS (f:fg) x counter = if f == x && counter == 1 then [f] ++ replaceS fg x 0
                            else if f == x && counter == 0 then replaceS fg x 0
                            else [f] ++ replaceS fg x 1
--flag s
remove1::Char -> IO ()
remove1 initialState = do end <- isEOF
                          if end
                             then putStr ""
                             else do linein <- getLine
                                     putStrLn (replaceS linein initialState 1 )
                                     remove1(initialState)

--check if character exist in set
if_exist [] _ = False
if_exist (f:fg) x = if (f == x) then True else if_exist fg x

-- (f:fg) <- set to change, replace-character to replace, s dictionary of banned word
light_remove::(Eq a)=>[a] -> a -> [a] -> [a]
light_remove [] _ _= []
light_remove (f:fg) replace s = if (if_exist s f) == True then
                                [f] ++ light_remove fg replace s else
                                [replace] ++ light_remove fg replace s


-- complete flag
complet::([String], [Char]) -> IO ()
complet initialState = do end <- isEOF
                          if end
                             then putStr (snd initialState)
                             else do linein <- getLine
                                     let args = fst (initialState)
                                     let dictionary = head (args)
                                     let replace = head(args !! 1)
                                     let result = light_remove linein replace dictionary
                                     let z = (snd initialState) ++ result
                                     complet(args, z)

--dictionaries
checkDic "[:alnum:]" = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
checkDic "[:alpha:]" = ['a'..'z'] ++ ['A'..'Z']
checkDic "[:digit:]" = ['0'..'9']
checkDic "[:lower:]" = ['a'..'z']
checkDic "[:upper:]" = ['A'..'Z']
checkDic x = x

main = do
    args <- getArgs
    if args == ["-v"] then version
    else if head (args) == "-t" then ( helper (tail args) )
    else if head (args) == "-d" then deleter (convert_arr_string_to_string(tail args))
    else if head (args) == "-s" then remove1 (head (convert_arr_string_to_string(tail args)))
    else if head (args) == "-c" || head(args) == "-C" then complet ([args !! 1, args !! 2], "")
    else if length(args) == 2 then (transl args)
    else print $ args
