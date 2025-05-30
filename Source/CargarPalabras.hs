module Source.CargarPalabras (cargarPalabras) where

-- Lee el archivo de texto y devuelve una lista de palabras (una por línea).
cargarPalabras :: IO [String]
cargarPalabras = do
    contenido <- readFile "palabras.txt"
    return (filter (not . null) (lines contenido)) 