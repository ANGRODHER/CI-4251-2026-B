module Main (main) where

-- Importamos tu función desde el archivo de las respuestas
import Test1.Sol (playChess) 

main :: IO ()
main = do
    putStrLn "¡Bienvenido a ProFunAVan Chess!"
    playChess