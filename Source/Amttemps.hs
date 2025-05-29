--modulo de intentos 

amttemps :: Int -> String -> IO()
amttemps ntry word = do
    
    --verificamos cuantos intentos tenemos disponibles 
    if ntry > 0 
        then do 
            putStrLn(word)
            -- Aquí se debe mostrar la función mostrar la palabra 

            --mostramos el numero de intentos 
            putStrLn("Intentos " ++ (show ntry) ++ "/6")

            --aquí se le da chance al usuairo para poder ingresar su siguiente intento 


            --se evalua la condición a ver si la logra adivinar 

            --por el momento no lo va a adibinar y llamamos nuevamente 
            amttemps (ntry-1) word
        
        else do
            putStrLn("se te acabaron los intentos")
    

main :: IO()
main = do
    amttemps 6 "hola"


