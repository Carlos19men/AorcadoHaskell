module Source.Menu(menu) where

import Source.DisguiseWord
import Source.WrongLetters
import Source.DibujoAhorcado
import Data.Char (toUpper, isLetter)
import Source.CargarPalabras (cargarPalabras)
import System.Random (randomRIO)
import Source.Estadisticas (actualizarEstadisticasJugador, mostrarEstadisticasJugador, 
    mostrarEstadisticas, obtenerEstadisticasJugador, cargarEstadisticas, jugadores, 
    EstadisticasJugador(..))
import Data.List (find)
import System.IO (hFlush, stdout)

-- Función para limpiar la pantalla usando caracteres de escape ANSI
limpiarPantalla :: IO ()
limpiarPantalla = putStr "\ESC[2J\ESC[H"

-- Nombre del jugador actual
data EstadoJuego = EstadoJuego {
    jugadorActual :: String  
} deriving (Show) -- Se puede convertir a string para imprimirlo

-- Estado inicial
estadoInicial :: EstadoJuego
estadoInicial = EstadoJuego ""  

-- Verificar si un nombre de jugador ya existe
nombreExiste :: String -> IO Bool
nombreExiste nombreBuscado = do
    est <- cargarEstadisticas
    -- Con any se verifica si el nombreBuscado existe en la lista de jugadores
    return $ any (\j -> nombre j == nombreBuscado) (jugadores est) -- jugadores est es la lista de todos los jugadores

-- Función para registrar o iniciar sesión
login :: IO String
login = do
    limpiarPantalla
    putStrLn "\n=== BIENVENIDO AL AHORCADO ==="
    putStrLn "1. Iniciar sesión"
    putStrLn "2. Registrarse"
    putStrLn "3. Salir del juego"
    putStr "Opción: "
    hFlush stdout -- Forzar la salida del mensaje de opción antes de leer
    opcion <- getLine -- Guarda la opción ingresada por el usuario
    case opcion of
        "1" -> do
            limpiarPantalla
            putStr "Por favor, ingrese su nombre de usuario: "
            hFlush stdout -- Forzar la salida del mensaje de opción antes de leer
            nombreIngresado <- getLine -- Guarda el nombre ingresado por el usuario
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
            limpiarPantalla
            putStr "Por favor, elija un nombre de usuario: "
            hFlush stdout -- Forzar la salida del mensaje antes de leer
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

-- Función que elige una palabra aleatoria de la lista de palabras.
palabraAleatoria :: [String] -> IO String
palabraAleatoria palabras = do
    i <- randomRIO (0, (length palabras) - 1) -- Se elige un indice aleatorio i de la lista de palabras
    return (palabras !! i) -- Se retorna la palabra en la posición i de la lista de palabras

-- Función para jugar una partida
jugarPartida :: String -> IO ()
jugarPartida nombreJugador = do
    limpiarPantalla
    putStrLn $ "\n¡Bienvenido al juego del ahorcado, " ++ nombreJugador ++ "!"
    palabras <- cargarPalabras
    palabra <- palabraAleatoria palabras
    putStrLn $ "La palabra tiene " ++ show (length palabra) ++ " letras"
    putStrLn "Tienes 6 intentos. ¡Buena suerte!"
    jugarTurno palabra [] 6 nombreJugador

-- Función para validar si un carácter es una letra válida
esLetraValida :: Char -> Bool
esLetraValida c = isLetter c

-- Función para jugar una partida
jugarTurno :: String -> [Char] -> Int -> String -> IO ()
jugarTurno palabra letrasUsadas intentos nombreJugador = do
    if intentos <= 0 then do
        limpiarPantalla  -- Aseguramos que la pantalla se limpie antes de mostrar el mensaje de derrota
        putStrLn "\n¡SE ACABARON TUS INTENTOS!"
        putStrLn $ dibujarAhorcado 0
        putStrLn $ "La palabra era: " ++ palabra
        putStrLn "\nPresiona Enter para continuar..."
        hFlush stdout
        _ <- getLine
        actualizarEstadisticasJugador nombreJugador "perdida"
        menuConJugador nombreJugador
    else do
        putStrLn $ "\n" ++ dibujarAhorcado intentos
        putStrLn $ "Letras Incorrectas: " ++ (wrongLetters letrasUsadas palabra)
        putStrLn $ "Intentos restantes: " ++ show intentos ++ "/6"
        putStrLn $ "Palabra: " ++ mostrarPalabra palabra letrasUsadas
        putStrLn "(Escribe '0' para abandonar la partida)"
        putStr "Ingresa una letra: "
        hFlush stdout
        letra <- getLine
        case letra of -- Se verifica la letra ingresada por el usuario
            "0" -> do
                limpiarPantalla
                putStrLn "\nHAS ABANDONADO LA PARTIDA"
                putStrLn $ dibujarAhorcado intentos
                putStrLn $ "La palabra era: " ++ palabra
                putStrLn "\nPresiona Enter para continuar..."
                hFlush stdout
                _ <- getLine
                actualizarEstadisticasJugador nombreJugador "abandonada"
                menuConJugador nombreJugador
            [c] -> do -- Se verifica si la letra ingresada es una lista de un caracter
                if not (esLetraValida c) then do 
                    putStrLn "Error: Por favor ingresa solo letras del alfabeto"
                    putStrLn "Presiona Enter para continuar..."
                    hFlush stdout
                    _ <- getLine
                    limpiarPantalla
                    jugarTurno palabra letrasUsadas intentos nombreJugador
                else do
                    let letraMayuscula = toUpper c
                    if letraMayuscula `elem` letrasUsadas then do -- Si la letra está en las letras usadas
                        putStrLn "\n¡Ya usaste esa letra!"
                        putStrLn "Presiona Enter para continuar..."
                        hFlush stdout
                        _ <- getLine
                        limpiarPantalla
                        jugarTurno palabra letrasUsadas intentos nombreJugador
                    else if letraMayuscula `elem` palabra then do -- Si la letra está en la palabra 
                        putStrLn "¡Correcto!"
                        if todasLetrasDescubiertas palabra (letraMayuscula:letrasUsadas) then do
                            limpiarPantalla
                            putStrLn "\n¡FELICIDADES! ¡HAS GANADO!"
                            putStrLn $ dibujarAhorcado intentos
                            putStrLn $ "La palabra era: " ++ palabra
                            putStrLn "\nPresiona Enter para continuar..."
                            hFlush stdout
                            _ <- getLine
                            actualizarEstadisticasJugador nombreJugador "ganada" -- Se actualiza la estadística de la partida ganada
                            menuConJugador nombreJugador -- Se llama a la función menuConJugador con el nombre del jugador
                        else do
                            limpiarPantalla
                            jugarTurno palabra (letraMayuscula:letrasUsadas) intentos nombreJugador -- Se llama a la función jugarTurno con la palabra, las letras usadas, el número de intentos y el nombre del jugador
                    else do
                        putStrLn "¡Incorrecto!"
                        putStrLn "Presiona Enter para continuar..."
                        hFlush stdout
                        _ <- getLine
                        limpiarPantalla
                        jugarTurno palabra (letraMayuscula:letrasUsadas) (intentos - 1) nombreJugador -- Se resta un intento
            _ -> do
                putStrLn "Error: Por favor ingresa una sola letra"
                putStrLn "Presiona Enter para continuar..."
                hFlush stdout
                _ <- getLine
                limpiarPantalla
                jugarTurno palabra letrasUsadas intentos nombreJugador

mostrarPalabra :: String -> [Char] -> String -- Muestra la palabra con las letras usadas 
mostrarPalabra palabra letrasUsadas = 
    -- \c representa a cada carácter de la palabra
    -- [c] ++ " " convierte la letra en una lista y agrega un espacio, en caso de que no esté en la palabra, retorna guión bajo
    concatMap (\c -> if c `elem` letrasUsadas then [c] ++ " " else "_ ") palabra  

-- Verifica que todas las letras usadas forman parte de palabra, si es así, la palabra fue descubierta
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
    limpiarPantalla
    putStrLn $ "\n=== MENÚ PRINCIPAL - " ++ nombreJugador ++ " ==="
    putStrLn "1. Jugar nueva partida"
    putStrLn "2. Ver mis estadísticas"
    putStrLn "3. Ver estadísticas de todos los jugadores"
    putStrLn "4. Cerrar sesión"
    putStrLn "5. Salir"
    putStr "Opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> jugarPartida nombreJugador
        "2" -> do
            limpiarPantalla
            mostrarEstadisticasJugador nombreJugador
            esperarTecla
            menuConJugador nombreJugador
        "3" -> do
            limpiarPantalla
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
