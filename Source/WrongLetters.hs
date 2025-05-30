module Source.WrongLetters (wrongLetters) where

-- Función recursiva (agrega coma a cada letra)
letter :: Char -> String
letter chr = [chr] ++ ","

-- Quita la última coma de la cadena
quit :: String -> String
quit xs = take ((length xs) - 1) xs

-- Letras incorrectas: solo se muestran las letras de xs que no están en la palabra.
wrongLetters :: String -> String -> String
wrongLetters xs palabra = "[" ++ quit (concat (map letter (filter (\c -> not (c `elem` palabra)) xs))) ++ "]"
