module Source.DibujoAhorcado (dibujarAhorcado) where

-- Dibuja el ahorcado según el número de intentos fallidos
dibujarAhorcado :: Int -> String
dibujarAhorcado intentos = case intentos of
    6 -> "  +---+\n  |   |\n      |\n      |\n      |\n      |\n========="
    5 -> "  +---+\n  |   |\n  O   |\n      |\n      |\n      |\n========="
    4 -> "  +---+\n  |   |\n  O   |\n  |   |\n      |\n      |\n========="
    3 -> "  +---+\n  |   |\n  O   |\n /|   |\n      |\n      |\n========="
    2 -> "  +---+\n  |   |\n  O   |\n /|\\  |\n      |\n      |\n========="
    1 -> "  +---+\n  |   |\n  O   |\n /|\\  |\n /    |\n      |\n========="
    0 -> "  +---+\n  |   |\n  O   |\n /|\\  |\n / \\  |\n      |\n========="
    _ -> "  +---+\n  |   |\n      |\n      |\n      |\n      |\n=========" 