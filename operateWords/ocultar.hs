--esta en una función para las palabras 
--convertirlas a guiones bajos 

module ModuleDisguise(disguise) where


--separar 
separate :: Char -> String 
separate letter = [letter] ++ " "

--convertir 
convert :: Char -> Char
convert letter = '_'

--ocultar
disguise :: String -> String
disguise str = concat (map separate (map convertir str))



