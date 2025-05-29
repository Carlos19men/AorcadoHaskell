--para compilar el proyecto hay que ejecutar: ghc -i./Source main.hs
--para ejecutar el proyecto hay que ejecutar: ./main
--module main 
module Main where 

import Source.Menu (menu)

main :: IO ()
main = menu
    