-- app/Main.hs
module Main where

import Engine.Types
import Engine.Parser
import Engine.Core
import Engine.Persistence
import System.IO

main :: IO ()
main = do
    -- Cargar el mundo
    -- Manejar el resultado de la carga
    -- Crear el estado inicial
    -- Iniciar el bucle del juego
    undefined


-- El bucle principal del juego
gameLoop :: GameState -> IO ()
gameLoop state = do
    -- Parsear la entrada del usuario a un Command
    -- Si hay error, continuar con el mismo estado
    -- Procesar el comando
    -- Mostrar resultado
    -- Continuar el bucle con el nuevo estado
    undefined