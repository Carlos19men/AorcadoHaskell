module Source.CargarPalabras (cargarPalabras) where

-- Lee el archivo de texto y devuelve una lista de palabras (una por línea).
-- filter (not . null) elimina las lineas vacias
-- not .null verifica si una linea no está vacía
cargarPalabras :: IO [String]
cargarPalabras = do
    contenido <- readFile "palabras.txt"
    return (filter (not . null) (lines contenido))  -- lines convierte el contenido en una lista de lineas
