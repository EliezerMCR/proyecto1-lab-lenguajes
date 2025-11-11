module Main where
import Test.HUnit
import Engine.Types
import Engine.Parser
import Engine.Core
import qualified Data.Map as Map

-- Pruebas para el parser
testParseCommand :: Test
testParseCommand = TestList
  [ "ir norte" ~: parseCommand "ir norte" ~?= Just (Ir Norte)
  , "tomar llave" ~: parseCommand "tomar llave" ~?= Just (Tomar "llave")
  , "coger espada" ~: parseCommand "coger espada" ~?= Just (Tomar "espada")
  , "mirar" ~: parseCommand "mirar" ~?= Just Mirar
  , "inventario" ~: parseCommand "inventario" ~?= Just Inventario
  , "inv" ~: parseCommand "inv" ~?= Just Inventario
  , "salir" ~: parseCommand "salir" ~?= Just Salir
  , "comando inválido" ~: parseCommand "comando inválido" ~?= Nothing
  ]

-- Pruebas para la lógica del juego
testProcessCommand :: Test
testProcessCommand = TestList
  [
    -- Prueba para el comando "mirar"
    let
      room = Room
        { roomName = "Sala Oscura"
        , roomDesc = "Una sala oscura."
        , roomObjects = []
        , roomExits = Map.empty
        }
      state = GameState
        { currentRoom = "Sala Oscura"
        , inventory = Map.empty
        , worldRooms = Map.singleton "Sala Oscura" room
        , worldItems = Map.empty
        }
      (msg, _) = processCommand Mirar state
    in "mirar sala" ~: msg ~?= "Una sala oscura."

    -- Prueba para el comando "tomar" (objeto no disponible)
  , let
      room = Room
        { roomName = "Sala con Objeto"
        , roomDesc = "Una sala con un objeto."
        , roomObjects = ["espada"]
        , roomExits = Map.empty
        }
      item = Item { itemName = "espada", itemDesc = "Una espada afilada." }
      state = GameState
        { currentRoom = "Sala con Objeto"
        , inventory = Map.empty
        , worldRooms = Map.singleton "Sala con Objeto" room
        , worldItems = Map.singleton "espada" item
        }
      (msg, _) = processCommand (Tomar "poción") state
    in "tomar objeto no disponible" ~: msg ~?= "No hay ningún objeto llamado 'poción' aquí. Los objetos disponibles son: espada."

    -- Prueba para el comando "tomar" (objeto disponible)
  , let
      room = Room
        { roomName = "Sala con Objeto"
        , roomDesc = "Una sala con un objeto."
        , roomObjects = ["espada"]
        , roomExits = Map.empty
        }
      item = Item { itemName = "espada", itemDesc = "Una espada afilada." }
      state = GameState
        { currentRoom = "Sala con Objeto"
        , inventory = Map.empty
        , worldRooms = Map.singleton "Sala con Objeto" room
        , worldItems = Map.singleton "espada" item
        }
      (msg, newState) = processCommand (Tomar "espada") state
    in "tomar objeto disponible" ~: msg ~?= "Has tomado: Una espada afilada."

    -- Prueba para el comando "ir" (dirección no disponible)
  , let
      room = Room
        { roomName = "Sala con Salida"
        , roomDesc = "Una sala con una salida."
        , roomObjects = []
        , roomExits = Map.singleton Norte "Otra Sala"
        }
      state = GameState
        { currentRoom = "Sala con Salida"
        , inventory = Map.empty
        , worldRooms = Map.singleton "Sala con Salida" room
        , worldItems = Map.empty
        }
      (msg, _) = processCommand (Ir Sur) state
    in "ir dirección no disponible" ~: msg ~?= "No puedes ir en esa dirección. Las salidas disponibles son: Norte."
  ]

-- Función principal para ejecutar las pruebas
main :: IO ()
main = do
  runTestTT testParseCommand
  runTestTT testProcessCommand
  return ()

