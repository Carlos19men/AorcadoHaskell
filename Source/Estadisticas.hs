{-# LANGUAGE ScopedTypeVariables #-}
module Source.Estadisticas where

import System.IO
import System.Directory (doesFileExist)
import Control.Exception (handle, SomeException)
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (fromMaybe)

-- Tipo para representar las estadísticas de un jugador
data EstadisticasJugador = EstadisticasJugador {
    nombre :: String,
    partidasGanadas :: Int,
    partidasPerdidas :: Int,
    partidasAbandonadas :: Int
} deriving (Show, Read)

-- Tipo para representar todas las estadísticas
data Estadisticas = Estadisticas {
    jugadores :: [EstadisticasJugador]
} deriving (Show, Read)

-- Nombre del archivo donde se guardarán las estadísticas
archivoEstadisticas :: FilePath
archivoEstadisticas = "estadisticas.txt"

-- Estadísticas iniciales
estadisticasIniciales :: Estadisticas
estadisticasIniciales = Estadisticas []

-- Estadísticas iniciales para un nuevo jugador
estadisticasJugadorIniciales :: String -> EstadisticasJugador
estadisticasJugadorIniciales nombre = EstadisticasJugador nombre 0 0 0

-- Cargar estadísticas desde el archivo
cargarEstadisticas :: IO Estadisticas
cargarEstadisticas = handle (\(_ :: SomeException) -> return estadisticasIniciales) $ do
    existe <- doesFileExist archivoEstadisticas
    if not existe then do
        withFile archivoEstadisticas WriteMode $ \h -> do
            hPutStrLn h (show estadisticasIniciales)
            hFlush h
        return estadisticasIniciales
    else do
        withFile archivoEstadisticas ReadMode $ \h -> do
            contenido <- hGetContents h
            case reads contenido of
                [(est, _)] -> return est
                _ -> return estadisticasIniciales

-- Guardar estadísticas en el archivo
guardarEstadisticas :: Estadisticas -> IO ()
guardarEstadisticas est = handle (\(_ :: SomeException) -> return ()) $ do
    withFile archivoEstadisticas WriteMode $ \h -> do
        hPutStrLn h (show est)
        hFlush h

-- Obtener o crear estadísticas de un jugador
obtenerEstadisticasJugador :: String -> IO EstadisticasJugador
obtenerEstadisticasJugador nombreJugador = do
    est <- cargarEstadisticas
    case find (\j -> nombre j == nombreJugador) (jugadores est) of
        Just jugador -> return jugador
        Nothing -> do
            let nuevoJugador = estadisticasJugadorIniciales nombreJugador
            let nuevaEst = est { jugadores = jugadores est ++ [nuevoJugador] }
            guardarEstadisticas nuevaEst
            return nuevoJugador

-- Actualizar estadísticas de un jugador
actualizarEstadisticasJugador :: String -> String -> IO ()
actualizarEstadisticasJugador nombreJugador resultado = do
    est <- cargarEstadisticas
    let jugadorActual = fromMaybe (estadisticasJugadorIniciales nombreJugador) 
                        (find (\j -> nombre j == nombreJugador) (jugadores est))
    
    let jugadorActualizado = case resultado of
            "ganada" -> jugadorActual { partidasGanadas = partidasGanadas jugadorActual + 1 }
            "perdida" -> jugadorActual { partidasPerdidas = partidasPerdidas jugadorActual + 1 }
            "abandonada" -> jugadorActual { partidasAbandonadas = partidasAbandonadas jugadorActual + 1 }
            _ -> jugadorActual
    
    let nuevaEst = Estadisticas {
        jugadores = map (\j -> if nombre j == nombreJugador then jugadorActualizado else j) 
                    (jugadores est)
    }
    
    guardarEstadisticas nuevaEst
    
    -- Mostrar estadísticas actualizadas
    putStrLn $ "\n=== Estadísticas Actualizadas para " ++ nombreJugador ++ " ==="
    putStrLn $ "Partidas ganadas: " ++ show (partidasGanadas jugadorActualizado)
    putStrLn $ "Partidas perdidas: " ++ show (partidasPerdidas jugadorActualizado)
    putStrLn $ "Partidas abandonadas: " ++ show (partidasAbandonadas jugadorActualizado)
    putStrLn "===========================================\n"

-- Mostrar estadísticas de un jugador
mostrarEstadisticasJugador :: String -> IO ()
mostrarEstadisticasJugador nombreJugador = do
    jugador <- obtenerEstadisticasJugador nombreJugador
    putStrLn $ "\n=== ESTADÍSTICAS DE " ++ nombreJugador ++ " ==="
    putStrLn $ "Partidas ganadas: " ++ show (partidasGanadas jugador)
    putStrLn $ "Partidas perdidas: " ++ show (partidasPerdidas jugador)
    putStrLn $ "Partidas abandonadas: " ++ show (partidasAbandonadas jugador)
    let total = partidasGanadas jugador + partidasPerdidas jugador + partidasAbandonadas jugador
    putStrLn $ "Total de partidas: " ++ show total
    putStrLn "================================\n"

-- Mostrar estadísticas de todos los jugadores
mostrarEstadisticas :: IO ()
mostrarEstadisticas = handle (\(_ :: SomeException) -> do
    putStrLn "\n=== ESTADÍSTICAS DEL JUEGO ==="
    putStrLn "Error al leer las estadísticas"
    putStrLn "=============================\n") $ do
    est <- cargarEstadisticas
    if null (jugadores est) then do
        putStrLn "\n=== ESTADÍSTICAS DEL JUEGO ==="
        putStrLn "No hay jugadores registrados aún"
        putStrLn "=============================\n"
    else do
        putStrLn "\n=== ESTADÍSTICAS DE TODOS LOS JUGADORES ==="
        mapM_ (\j -> do
            putStrLn $ "\nJugador: " ++ nombre j
            putStrLn $ "Partidas ganadas: " ++ show (partidasGanadas j)
            putStrLn $ "Partidas perdidas: " ++ show (partidasPerdidas j)
            putStrLn $ "Partidas abandonadas: " ++ show (partidasAbandonadas j)
            let total = partidasGanadas j + partidasPerdidas j + partidasAbandonadas j
            putStrLn $ "Total de partidas: " ++ show total
            putStrLn "----------------------------") (jugadores est)
        putStrLn "===========================================\n" 