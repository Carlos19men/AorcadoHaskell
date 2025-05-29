{-# LANGUAGE ScopedTypeVariables #-}
module Source.Estadisticas where

import System.IO
import System.Directory (doesFileExist)
import Control.Exception (handle, SomeException, IOException, try)
import Control.Monad (when)

-- Tipo para representar las estadísticas
data Estadisticas = Estadisticas {
    partidasGanadas :: Int,
    partidasPerdidas :: Int,
    partidasAbandonadas :: Int
} deriving (Show, Read)

-- Nombre del archivo donde se guardarán las estadísticas
archivoEstadisticas :: FilePath
archivoEstadisticas = "estadisticas.txt"

-- Estadísticas iniciales
estadisticasIniciales :: Estadisticas
estadisticasIniciales = Estadisticas 0 0 0

-- Cargar estadísticas desde el archivo
cargarEstadisticas :: IO Estadisticas
cargarEstadisticas = handle (\(_ :: SomeException) -> return estadisticasIniciales) $ do
    existe <- doesFileExist archivoEstadisticas
    if not existe then do
        -- Si el archivo no existe, lo creamos con estadísticas iniciales
        withFile archivoEstadisticas WriteMode $ \h -> do
            hPutStrLn h (show estadisticasIniciales)
            hFlush h
        return estadisticasIniciales
    else do
        -- Leemos el archivo de manera segura
        withFile archivoEstadisticas ReadMode $ \h -> do
            contenido <- hGetContents h
            case reads contenido of
                [(est, _)] -> return est
                _ -> return estadisticasIniciales

-- Actualizar estadísticas según el resultado
actualizarEstadisticas :: String -> IO ()
actualizarEstadisticas resultado = do
    -- Leemos las estadísticas actuales
    est <- cargarEstadisticas
    
    -- Calculamos las nuevas estadísticas
    let nuevaEst = case resultado of
            "ganada" -> Estadisticas {
                partidasGanadas = partidasGanadas est + 1,
                partidasPerdidas = partidasPerdidas est,
                partidasAbandonadas = partidasAbandonadas est
            }
            "perdida" -> Estadisticas {
                partidasGanadas = partidasGanadas est,
                partidasPerdidas = partidasPerdidas est + 1,
                partidasAbandonadas = partidasAbandonadas est
            }
            "abandonada" -> Estadisticas {
                partidasGanadas = partidasGanadas est,
                partidasPerdidas = partidasPerdidas est,
                partidasAbandonadas = partidasAbandonadas est + 1
            }
            _ -> est
    
    -- Escribimos las nuevas estadísticas de manera segura
    withFile archivoEstadisticas WriteMode $ \h -> do
        hPutStrLn h (show nuevaEst)
        hFlush h
    
    -- Mostramos las estadísticas actualizadas
    putStrLn "\n=== Estadísticas Actualizadas ==="
    putStrLn $ "Partidas ganadas: " ++ show (partidasGanadas nuevaEst)
    putStrLn $ "Partidas perdidas: " ++ show (partidasPerdidas nuevaEst)
    putStrLn $ "Partidas abandonadas: " ++ show (partidasAbandonadas nuevaEst)
    putStrLn "==============================\n"

-- Mostrar estadísticas en formato legible
mostrarEstadisticas :: IO ()
mostrarEstadisticas = handle (\(_ :: SomeException) -> do
    putStrLn "\n=== ESTADÍSTICAS DEL JUEGO ==="
    putStrLn "Error al leer las estadísticas"
    putStrLn "=============================\n") $ do
    est <- cargarEstadisticas
    putStrLn "\n=== ESTADÍSTICAS DEL JUEGO ==="
    putStrLn $ "Partidas ganadas: " ++ show (partidasGanadas est)
    putStrLn $ "Partidas perdidas: " ++ show (partidasPerdidas est)
    putStrLn $ "Partidas abandonadas: " ++ show (partidasAbandonadas est)
    let total = partidasGanadas est + partidasPerdidas est + partidasAbandonadas est
    putStrLn $ "Total de partidas: " ++ show total
    putStrLn "=============================\n" 