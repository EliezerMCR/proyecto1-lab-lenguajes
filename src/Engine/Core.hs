module Engine.Core (processCommand) where

import Data.Char (toLower)
import Data.List (find, intercalate)
import qualified Data.Map as Map
import Engine.Types

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
        Nothing ->
          let availableExits = Map.keys (roomExits room)
              exitNames = map show availableExits
          in if null exitNames
               then ("No hay salidas en esta sala.", state)
               else ("No puedes ir en esa dirección. Las salidas disponibles son: " ++ intercalate ", " exitNames ++ ".", state)
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
      case find (nameMatches objName) (roomObjects room) of
        Just actualName ->
          case Map.lookup actualName (worldItems state) of
            Nothing -> ("Error: El objeto no existe en el mundo.", state)
            Just item ->
              let newRoomObjects = filter (/= actualName) (roomObjects room)
                  newRoom = room { roomObjects = newRoomObjects }
                  newRooms = Map.insert (currentRoom state) newRoom (worldRooms state)
                  newInventory = Map.insert actualName item (inventory state)
                  newState = state { worldRooms = newRooms, inventory = newInventory }
                  msg = "Has tomado: " ++ itemDesc item
              in (msg, newState)
        Nothing ->
          let availableObjects = roomObjects room
          in if null availableObjects
               then ("No hay objetos en esta sala.", state)
               else ("No hay ningún objeto llamado '" ++ objName ++ "' aquí. Los objetos disponibles son: " ++ intercalate ", " availableObjects ++ ".", state)

-- Compara nombres de objetos sin diferenciar mayúsculas/minúsculas
nameMatches :: String -> String -> Bool
nameMatches expected actual =
  map toLower expected == map toLower actual


-- Maneja el comando de mostrar inventario
handleInventory :: GameState -> (String, GameState)
handleInventory state =
  let inv = inventory state
  in if Map.null inv
       then ("Tu inventario está vacío. ¡Explora para encontrar objetos!", state)
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