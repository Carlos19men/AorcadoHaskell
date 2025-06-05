--esta en una función para las palabras 
--convertirlas a guiones bajos 

module Source.DisguiseWord(disguise) where


--separar. Agregar un espacio al final de cada letra
separate :: Char -> String 
separate letter = [letter] ++ " "

--convertir. Convertir cada letra en un guion bajo
convert :: Char -> Char
convert letter = '_'

--ocultar. Ocultar las letras de la palabra
disguise :: String -> String
disguise str = concat (map separate (map convert str))



