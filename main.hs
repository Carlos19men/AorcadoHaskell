
--para compilar el proyecto hay que ejecutar: ghc -i./Source main.hs
--para ejecutar el proyecto hay que ejecutar: ./main
--module main 
module Main where 

import Source.DisguiseWord
import Source.ValidateInput

main :: IO()
main = do
    print( validateInput 'a') -- True
    print( validateInput '1') -- False
    print( validateInput '4')