module Engine.Types where

import qualified Data.Map as Map

-- Direcciones posibles en el juego
data Direction = Norte | Sur | Este | Oeste
  deriving (Show, Eq, Ord)

-- Un objeto en el juego
data Item = Item
  { itemName :: String
  , itemDesc :: String
  }
  deriving (Show, Eq)

-- Una sala en el juego
data Room = Room
  { roomName :: String
  , roomDesc :: String
  , roomExits :: Map.Map Direction String  -- Map de dirección -> nombre de sala destino
  , roomObjects :: [String]                -- Lista de nombres de objetos en la sala
  }
  deriving (Show, Eq)

-- El estado completo del juego
data GameState = GameState
  { currentRoom :: String                    -- Nombre de la sala actual
  , inventory :: Map.Map String Item         -- Inventario del jugador
  , worldRooms :: Map.Map String Room        -- Todas las salas del mundo
  , worldItems :: Map.Map String Item        -- Todos los items del mundo
  }
  deriving (Show, Eq)

-- Comandos que el jugador puede ejecutar
data Command
  = Ir Direction
  | Mirar
  | Tomar String
  | Coger String
  | Inventario
  | Inv
  | Salir
  deriving (Show, Eq)

-- Tipos para loadWorldData
type RoomContainer = Map.Map String Room
type ItemContainer = Map.Map String Item