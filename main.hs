
--para compilar el proyecto hay que ejecutar: ghc -i./Source main.hs
--para ejecutar el proyecto hay que ejecutar: ./main
--module main 
module Main where 

import Source.DisguiseWord
import Source.ValidateInput
import Source.Menu
import Source.WrongLetters
import Source.Amttemps

main :: IO()
main = do
    --función para obtener un solo caracter 
    print(wrongLetters "sadfasdf")
    