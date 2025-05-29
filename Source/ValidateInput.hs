--validar la entrada por pantalla 
module Source.ValidateInput (getInput,stringToChar) where

import Data.Char (toLower)


--validar que se un solo caracter
itsConvertibleChar :: String -> Bool
itsConvertibleChar str = (length str == 1)

--
stringToChar :: String -> Char
stringToChar [str] = str

--función para validar que el caracter ingresado es valido 
validate :: String -> Bool
validate chr = elem (toLower (stringToChar chr)) "abcdefghijklmnopqrstuvwxyz"

    
--función de petición de caracter 

getInput :: IO()
getInput = do   
    putStrLn "Ingresar un caracter (uno solo):"
    input <- getLine

    --evaluamos si la longitud de la cadena es de 1
    if itsConvertibleChar input
        then do 
            putStrLn "es un caracter"

            --lo convertimos en un char
            print(validate input)
        else do 
            putStrLn "Dato no valido\n"
            getInput

