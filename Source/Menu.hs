module Source.Menu(menu) where

import Source.DisguiseWord
import Source.WrongLetters
import Data.Char (toUpper)
import Source.CargarPalabras (cargarPalabras)
import System.Random (randomRIO)
import Source.Estadisticas (actualizarEstadisticas, mostrarEstadisticas)

-- Palabra a adivinar (se carga desde un archivo de texto)
-- palabraSecreta :: String
-- palabraSecreta = "HASKELL"

-- Función que elige una palabra aleatoria de la lista de palabras.
palabraAleatoria :: [String] -> IO String
palabraAleatoria palabras = do
    i <- randomRIO (0, (length palabras) - 1)
    return (palabras !! i)

-- Función para jugar una partida
jugarPartida :: IO ()
jugarPartida = do
    putStrLn "\n¡Bienvenido al juego del ahorcado!"
    palabras <- cargarPalabras
    palabra <- palabraAleatoria palabras
    putStrLn $ "La palabra tiene " ++ show (length palabra) ++ " letras"
    putStrLn $ "Palabra: " ++ disguise palabra
    putStrLn "\nTienes 6 intentos. ¡Buena suerte!"
    jugarTurno palabra [] 6

jugarTurno :: String -> [Char] -> Int -> IO ()
jugarTurno palabra letrasUsadas intentos = do
    if intentos <= 0 then do
        putStrLn "\n¡Se acabaron tus intentos!"
        putStrLn $ "La palabra era: " ++ palabra
        actualizarEstadisticas "perdida"
        menu
    else do
        putStrLn $ "\nLetras usadas: " ++ (wrongLetters letrasUsadas palabra)
        putStrLn $ "Intentos restantes: " ++ show intentos
        putStrLn $ "Palabra: " ++ mostrarPalabra palabra letrasUsadas
        putStrLn "(Escribe '0' para abandonar la partida)"
        putStr "Ingresa una letra: "
        letra <- getLine
        case letra of
            "0" -> do
                putStrLn "\nHas abandonado la partida"
                putStrLn $ "La palabra era: " ++ palabra
                putStrLn "Intentando actualizar estadísticas..."  -- Debug
                actualizarEstadisticas "abandonada"
                putStrLn "Estadísticas actualizadas"  -- Debug
                menu
            [c] | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] -> do
                let letraMayuscula = toUpper c
                if letraMayuscula `elem` letrasUsadas then do
                    putStrLn "¡Ya usaste esa letra!"
                    jugarTurno palabra letrasUsadas intentos
                else if letraMayuscula `elem` palabra then do
                    putStrLn "¡Correcto!"
                    if todasLetrasDescubiertas palabra (letraMayuscula:letrasUsadas) then do
                        putStrLn "\n¡Felicidades! ¡Has ganado!"
                        putStrLn $ "La palabra era: " ++ palabra
                        actualizarEstadisticas "ganada"
                        menu
                    else
                        jugarTurno palabra (letraMayuscula:letrasUsadas) intentos
                else do
                    putStrLn "¡Incorrecto!"
                    jugarTurno palabra (letraMayuscula:letrasUsadas) (intentos - 1)
            _ -> do
                putStrLn "Por favor ingresa una sola letra válida"
                jugarTurno palabra letrasUsadas intentos

mostrarPalabra :: String -> [Char] -> String
mostrarPalabra palabra letrasUsadas = 
    concatMap (\c -> if c `elem` letrasUsadas then [c] ++ " " else "_ ") palabra

todasLetrasDescubiertas :: String -> [Char] -> Bool
todasLetrasDescubiertas palabra letrasUsadas = 
    all (`elem` letrasUsadas) palabra

verEstadisticas :: IO ()
verEstadisticas = do
    mostrarEstadisticas
    menu

menu :: IO ()
menu = menuBody jugarPartida verEstadisticas

menuBody :: IO() -> IO() -> IO()
menuBody game static = do 
    putStrLn("Hola! elige una opción: ")
    putStrLn("1- Jugar nueva partida")
    putStrLn("2- Ver estadisticas")
    putStrLn("3- salir. \n")
    input <- getLine
    case input of
        "1" -> game
        "2" -> static
        "3" -> putStrLn("Hasta luego, vuelva pronto\n")
        _ -> do
            putStrLn("Opcion no valida, ingrese nuevamente\n")
            menuBody game static
