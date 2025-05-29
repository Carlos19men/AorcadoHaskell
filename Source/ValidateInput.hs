--validar la entrada por pantalla 
module Source.ValidateInput (validateInput) where

import Data.Char (toLower)


--función para validar que el caracter ingresado es valido 
validateInput :: Char -> Bool
validateInput char = elem (toLower char) "abcdefghijklmnopqrstuvwxyz"
