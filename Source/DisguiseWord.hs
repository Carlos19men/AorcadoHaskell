--esta en una función para las palabras 
--convertirlas a guiones bajos 

module Source.DisguiseWord(disguise) where


--separar 
separate :: Char -> String 
separate letter = [letter] ++ " "

--convertir 
convert :: Char -> Char
convert letter = '_'

--ocultar
disguise :: String -> String
disguise str = concat (map separate (map convert str))



