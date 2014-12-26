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


prepareTranslation aa bb to_translate = translate (zip aa bb) to_translate
