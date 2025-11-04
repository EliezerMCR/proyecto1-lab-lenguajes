-- app/Main.hs
module Main where

import Engine.Types
import Engine.Parser
import Engine.Core
import Engine.Persistence
import qualified Data.Map as Map
import System.IO

main :: IO ()
main = do
    putStrLn "==================================="
    putStrLn "Motor de Aventura de Texto"
    putStrLn "==================================="
    putStrLn ""

    -- Cargar el mundo desde mundo.txt
    result <- loadWorldData "mundo.txt"

    case result of
        Left errorMsg -> do
            putStrLn $ "Error al cargar el mundo: " ++ errorMsg
            putStrLn "El juego no puede iniciar."

        Right (rooms, items) -> do
            -- Verificar que hay al menos una sala
            if Map.null rooms
                then putStrLn "Error: El mundo no tiene salas definidas."
                else do
                    -- Crear el estado inicial
                    -- La sala inicial será la primera sala del mapa
                    let firstRoomName = fst $ head $ Map.toList rooms
                    let initialState = GameState
                            { currentRoom = firstRoomName
                            , inventory = Map.empty
                            , worldRooms = rooms
                            , worldItems = items
                            }

                    -- Mostrar descripción inicial de la sala
                    case Map.lookup firstRoomName rooms of
                        Nothing -> putStrLn "Error: No se pudo cargar la sala inicial."
                        Just room -> do
                            putStrLn "=== COMIENZA LA AVENTURA ==="
                            putStrLn ""
                            let (msg, _) = processCommand Mirar initialState
                            putStrLn msg
                            putStrLn ""

                            -- Iniciar el bucle del juego
                            gameLoop initialState


-- El bucle principal del juego
gameLoop :: GameState -> IO ()
gameLoop state = do
    -- Mostrar prompt
    putStr "> "
    hFlush stdout

    -- Leer entrada del usuario
    input <- getLine

    -- Parsear el comando
    case parseCommand input of
        Nothing -> do
            putStrLn "Comando no reconocido. Intenta: ir <dirección>, mirar, tomar <objeto>, inventario, salir"
            putStrLn ""
            gameLoop state

        Just cmd -> do
            -- Procesar el comando con la lógica pura
            let (msg, newState) = processCommand cmd state

            -- Mostrar el resultado
            putStrLn ""
            putStrLn msg
            putStrLn ""

            -- Si el comando fue Salir, terminar el bucle
            -- De lo contrario, continuar con el nuevo estado
            case cmd of
                Salir -> return ()
                _ -> gameLoop newState