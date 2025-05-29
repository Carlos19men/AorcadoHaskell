module Source.Menu(menu) where


--funciones para las diferentes opciones 
jugarPartida :: IO()
jugarPartida = putStrLn("estas jugado una partida")

verEstadisticas :: IO()
verEstadisticas = putStrLn("Estoy viendo estadisticas ")



--comparar input 
menuBody :: IO() -> IO() -> IO()
menuBody game static = do 
    --mostramos el menu 

    putStrLn("Hola! elige una opción: ")
    putStrLn("1- Jugar nueva partida")
    putStrLn("2- Ver estadisticas")
    putStrLn("3- salir. \n")

    input <- getLine

    --evaluamos las opciones 
    case input of
        "1" -> game
        "2" -> static
        "3" -> putStrLn("hasta Luego, vuelva pronto\n")
        _ -> do
            putStrLn("opcion no valida ingrese nuevamente\n")
            menuBody game static
        

menu :: IO()
menu = do
    menuBody jugarPartida verEstadisticas
