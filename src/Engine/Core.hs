module Engine.Core (processCommand) where

import Engine.Types
import qualified Data.Map as Map
import Data.List (intercalate)

-- La función PURA que actualiza el estado del juego
processCommand :: Command -> GameState -> (String, GameState)
processCommand command state =
  case command of
    Ir dir -> handleMove dir state
    Mirar -> handleLook state
    Tomar obj -> handleTake obj state
    Coger obj -> handleTake obj state -- Coger es sinónimo de Tomar
    Inventario -> handleInventory state
    Inv -> handleInventory state  -- Inv es sinónimo de Inventario
    Salir -> handleExit state

-- Maneja el comando de moverse en una dirección
handleMove :: Direction -> GameState -> (String, GameState)
handleMove dir state =
  case Map.lookup (currentRoom state) (worldRooms state) of
    Nothing -> ("Error: Sala actual no existe.", state)
    Just room ->
      case Map.lookup dir (roomExits room) of
        Nothing -> ("No puedes ir en esa dirección.", state)
        Just nextRoomName ->
          case Map.lookup nextRoomName (worldRooms state) of
            Nothing -> ("Error: La sala destino no existe.", state)
            Just nextRoom ->
              let newState = state { currentRoom = nextRoomName }
                  msg = describeRoom nextRoom
              in (msg, newState)

-- Maneja el comando de mirar la sala actual
handleLook :: GameState -> (String, GameState)
handleLook state =
  case Map.lookup (currentRoom state) (worldRooms state) of
    Nothing -> ("Error: Sala actual no existe.", state)
    Just room -> (describeRoom room, state)

-- Maneja el comando de tomar/coger un objeto
handleTake :: String -> GameState -> (String, GameState)
handleTake objName state =
  case Map.lookup (currentRoom state) (worldRooms state) of
    Nothing -> ("Error: Sala actual no existe.", state)
    Just room ->
      if objName `elem` roomObjects room
        then
          case Map.lookup objName (worldItems state) of
            Nothing -> ("Error: El objeto no existe en el mundo.", state)
            Just item ->
              let -- Remover objeto de la sala
                  newRoomObjects = filter (/= objName) (roomObjects room)
                  newRoom = room { roomObjects = newRoomObjects }
                  newRooms = Map.insert (currentRoom state) newRoom (worldRooms state)
                  -- Añadir objeto al inventario
                  newInventory = Map.insert objName item (inventory state)
                  newState = state { worldRooms = newRooms, inventory = newInventory }
                  msg = "Has tomado: " ++ itemDesc item
              in (msg, newState)
        else ("No hay ningún objeto llamado '" ++ objName ++ "' aquí.", state)

-- Maneja el comando de mostrar inventario
handleInventory :: GameState -> (String, GameState)
handleInventory state =
  let inv = inventory state
  in if Map.null inv
       then ("Tu inventario está vacío.", state)
       else
         let items = Map.elems inv
             itemList = map (\item -> "- " ++ itemName item ++ ": " ++ itemDesc item) items
             msg = "Inventario:\n" ++ intercalate "\n" itemList
         in (msg, state)

-- Maneja el comando de salir
handleExit :: GameState -> (String, GameState)
handleExit state =
  ("¡Gracias por jugar! Hasta pronto.", state)

-- Describe una sala con su descripción, objetos y salidas
describeRoom :: Room -> String
describeRoom room =
  let desc = roomDesc room
      objSection = if null (roomObjects room)
                     then ""
                     else "\n\nObjetos visibles: " ++ intercalate ", " (roomObjects room)
      exitSection = if Map.null (roomExits room)
                      then ""
                      else "\n\nSalidas: " ++ formatExits (roomExits room)
  in desc ++ objSection ++ exitSection

-- Formatea las salidas de una sala
formatExits :: Map.Map Direction String -> String
formatExits exits =
  let exitList = Map.toList exits
      formatted = map (\(dir, dest) -> show dir ++ " -> " ++ dest) exitList
  in intercalate ", " formatted