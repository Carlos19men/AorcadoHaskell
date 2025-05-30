module Source.Menu(menu) where

import Source.DisguiseWord
import Source.WrongLetters
import Data.Char (toUpper)
import Source.CargarPalabras (cargarPalabras)
import System.Random (randomRIO)
import Source.Estadisticas (actualizarEstadisticasJugador, mostrarEstadisticasJugador, 
    mostrarEstadisticas, obtenerEstadisticasJugador, cargarEstadisticas, jugadores, 
    EstadisticasJugador(..))
import Data.List (find)
import System.IO (hFlush, stdout)

-- Estado del jugador actual
data EstadoJuego = EstadoJuego {
    jugadorActual :: String  -- Ahora es simplemente String
} deriving (Show)

-- Estado inicial
estadoInicial :: EstadoJuego
estadoInicial = EstadoJuego ""  -- String vacío para indicar que no hay jugador

-- Verificar si un nombre de jugador ya existe
nombreExiste :: String -> IO Bool
nombreExiste nombreBuscado = do
    est <- cargarEstadisticas
    return $ any (\j -> nombre j == nombreBuscado) (jugadores est)

-- Función para registrar o iniciar sesión
login :: IO String
login = do
    putStrLn "\n=== BIENVENIDO AL AHORCADO ==="
    putStrLn "1. Iniciar sesión"
    putStrLn "2. Registrarse"
    putStrLn "3. Salir del juego"
    putStr "Opción: "
    hFlush stdout  -- Forzar la salida del prompt antes de leer
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Por favor, ingrese su nombre de usuario: "
            hFlush stdout  -- Forzar la salida del prompt antes de leer
            nombreIngresado <- getLine
            if null nombreIngresado then do
                putStrLn "\nError: El nombre no puede estar vacío"
                login
            else do
                existe <- nombreExiste nombreIngresado
                if existe then do
                    jugador <- obtenerEstadisticasJugador nombreIngresado
                    putStrLn $ "\n¡Bienvenido de nuevo, " ++ nombreIngresado ++ "!"
                    return nombreIngresado
                else do
                    putStrLn "\nError: Este nombre no está registrado"
                    login
        "2" -> do
            putStr "Por favor, elija un nombre de usuario: "
            hFlush stdout  -- Forzar la salida del prompt antes de leer
            nombreIngresado <- getLine
            if null nombreIngresado then do
                putStrLn "\nError: El nombre no puede estar vacío"
                login
            else do
                existe <- nombreExiste nombreIngresado
                if existe then do
                    putStrLn "\nError: Este nombre ya está registrado"
                    putStrLn "Por favor, elija otro nombre o inicie sesión: "
                    login
                else do
                    jugador <- obtenerEstadisticasJugador nombreIngresado
                    putStrLn $ "\n¡Bienvenido, " ++ nombreIngresado ++ "!"
                    putStrLn "Su cuenta ha sido creada exitosamente"
                    return nombreIngresado
        "3" -> do
            putStrLn "\nGracias por jugar. ¡Hasta pronto!"
            return ""
        _ -> do
            putStrLn "\nError: Opción no válida"
            login

-- Palabra a adivinar (se carga desde un archivo de texto)
-- palabraSecreta :: String
-- palabraSecreta = "HASKELL"

-- Función que elige una palabra aleatoria de la lista de palabras.
palabraAleatoria :: [String] -> IO String
palabraAleatoria palabras = do
    i <- randomRIO (0, (length palabras) - 1)
    return (palabras !! i)

-- Función para jugar una partida
jugarPartida :: String -> IO ()
jugarPartida nombreJugador = do
    putStrLn $ "\n¡Bienvenido al juego del ahorcado, " ++ nombreJugador ++ "!"
    palabras <- cargarPalabras
    palabra <- palabraAleatoria palabras
    putStrLn $ "La palabra tiene " ++ show (length palabra) ++ " letras"
    putStrLn $ "Palabra: " ++ disguise palabra
    putStrLn "\nTienes 6 intentos. ¡Buena suerte!"
    jugarTurno palabra [] 6 nombreJugador

jugarTurno :: String -> [Char] -> Int -> String -> IO ()
jugarTurno palabra letrasUsadas intentos nombreJugador = do
    if intentos <= 0 then do
        putStrLn "\n¡Se acabaron tus intentos!"
        putStrLn $ "La palabra era: " ++ palabra
        actualizarEstadisticasJugador nombreJugador "perdida"
        menuConJugador nombreJugador
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
                putStrLn "Actualizando estadísticas..."
                actualizarEstadisticasJugador nombreJugador "abandonada"
                menuConJugador nombreJugador
            [c] -> do
                let letraMayuscula = toUpper c
                if letraMayuscula `elem` letrasUsadas then do
                    putStrLn "¡Ya usaste esa letra!"
                    jugarTurno palabra letrasUsadas intentos nombreJugador
                else if letraMayuscula `elem` palabra then do
                    putStrLn "¡Correcto!"
                    if todasLetrasDescubiertas palabra (letraMayuscula:letrasUsadas) then do
                        putStrLn "\n¡Felicidades! ¡Has ganado!"
                        putStrLn $ "La palabra era: " ++ palabra
                        actualizarEstadisticasJugador nombreJugador "ganada"
                        menuConJugador nombreJugador
                    else
                        jugarTurno palabra (letraMayuscula:letrasUsadas) intentos nombreJugador
                else do
                    putStrLn "¡Incorrecto!"
                    jugarTurno palabra (letraMayuscula:letrasUsadas) (intentos - 1) nombreJugador
            _ -> do
                putStrLn "Por favor ingresa una sola letra válida"
                jugarTurno palabra letrasUsadas intentos nombreJugador

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

-- Función para esperar que el usuario presione una tecla
esperarTecla :: IO ()
esperarTecla = do
    putStrLn "\nPresione Enter para continuar..."
    hFlush stdout
    _ <- getLine
    return ()

-- Menú principal con jugador
menuConJugador :: String -> IO ()
menuConJugador nombreJugador = do
    putStrLn $ "\n=== MENÚ PRINCIPAL - " ++ nombreJugador ++ " ==="
    putStrLn "1. Jugar nueva partida"
    putStrLn "2. Ver mis estadísticas"
    putStrLn "3. Ver estadísticas de todos los jugadores"
    putStrLn "4. Cerrar sesión"
    putStrLn "5. Salir"
    putStr "Opción: "
    hFlush stdout  -- Forzar la salida del prompt antes de leer
    opcion <- getLine
    case opcion of
        "1" -> jugarPartida nombreJugador
        "2" -> do
            mostrarEstadisticasJugador nombreJugador
            esperarTecla
            menuConJugador nombreJugador
        "3" -> do
            mostrarEstadisticas
            esperarTecla
            menuConJugador nombreJugador
        "4" -> menu
        "5" -> putStrLn "¡Hasta luego!"
        _ -> do
            putStrLn "Opción inválida."
            menuConJugador nombreJugador

-- Menú principal
menu :: IO ()
menu = do
    nombreJugador <- login
    if nombreJugador == "" then
        return ()
    else
        menuConJugador nombreJugador
