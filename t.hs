
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

helper :: (Show x, Eq x)=>[x] -> IO ()
helper initialState = do end <- isEOF
                         if end
                             then putStr ""
                             else do linein <- getLine
                                     let a = show (initialState !! 0)
                                     let z = show ((initialState !! 1))
                                     putStrLn (prepareTranslation a z linein )
                                     helper(initialState)

transition :: (String, String) -> (String, String)
transition inpair = (nextState, nextState) where nextState = (snd inpair) ++ (fst inpair)  
