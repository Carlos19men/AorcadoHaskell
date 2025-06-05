module Source.WrongLetters (wrongLetters) where

-- La función toma un caracter y le agrega una coma
letter :: Char -> String
letter chr = [chr] ++ ","

-- Quita la última coma de la cadena
quit :: String -> String
quit xs = take ((length xs) - 1) xs

-- Solo muestra las letras incorrectas que no sean parte de la palabra.
wrongLetters :: String -> String -> String
wrongLetters xs palabra = "Letras incorrectas: [" ++ quit (concat (map letter (filter (\c -> not (c `elem` palabra)) xs))) ++ "]"
