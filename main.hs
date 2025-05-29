
--module main 
module Main where 

import Source.DisguiseWord
import Source.ValidateInput

main :: IO()
main = do
    print( validateInput 'a') -- True
    print( validateInput '1') -- False