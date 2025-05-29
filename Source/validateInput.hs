import Data.Char (toLower)

--validar la entrada por pantalla 

module Source.ValidateInput (validateInput) where
    
--función para validar que el caracter ingresado es valido 
validateInput :: Char -> Bool
validateInput char = elem (toLower char) "abcdefghijklmnopqrstuvwxyz"
