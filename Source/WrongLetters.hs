module Source.WrongLetters(wrongLetters) where

--las letras incorrectas se van concatenando 

--función recursiva 
letter :: Char -> String
letter chr = [chr]++","

--quitar la ultima com
quit :: String -> String
quit xs = take ((length xs) - 1) xs

--letras incorrectas
wrongLetters :: String -> String
wrongLetters xs =  "Letras incorrectas: [" ++ (quit (concat (map letter xs))) ++ "]"
