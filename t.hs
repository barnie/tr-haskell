
import Control.Monad (unless)
import System.IO (isEOF)


replace1::(Eq a)=>a->a->[a]->[a]
replace1 _ _ [] = []
replace1 x y (f:fg) = if x == f then [y] ++ (replace1 x y fg) else [f] ++ (replace1 x y fg)

--translate [] [] lista = lista
--translate (f:fx) (g:gx) lista = translate fx gx (replace1 f g lista)
--nie dziala dla "abc" "bcd"
--
ifExist x [] = []
ifExist x (f:fg) = if ( x == (fst f)) then [f] else ifExist x fg

translate _ [] = []
translate ds (f:fg) = if (null (ifExist f ds)) == True then [f] ++ (translate ds fg) else [ ( snd (head (ifExist f ds)) ) ] ++ (translate ds fg)

prepareTranslation::(Show a, Eq a)=>[a]->[a]->[a]->[a]
prepareTranslation aa bb to_translate = translate (zip aa bb) to_translate

processEachLine :: (String -> IO a) -> IO ()
processEachLine k = do
        finished <- isEOF
        unless finished $ do
                k =<< getLine
                processEachLine k

main :: IO ()
main = helper(["abc", "ABC"]) --initial state is empty string

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
