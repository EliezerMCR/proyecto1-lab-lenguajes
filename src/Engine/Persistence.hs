module Engine.Persistence (loadWorldData) where

import Engine.Types
import qualified Data.Map as Map
import Data.List (stripPrefix)
import Data.Char (toLower)
import Data.Maybe (mapMaybe)

-- Carga el archivo del mundo.
-- Devuelve (IO (Either Error (MapaDeSalas, MapaDeItems)))
loadWorldData :: FilePath -> IO (Either String (RoomContainer, ItemContainer))
loadWorldData filePath = do
  content <- readFile filePath
  return $ parseWorldFile content

-- Parsea el contenido completo del archivo
parseWorldFile :: String -> Either String (RoomContainer, ItemContainer)
parseWorldFile content = do
  let blocks = splitBlocks content
  let (itemBlocks, roomBlocks) = partitionBlocks blocks
  items <- parseItems itemBlocks
  rooms <- parseRooms roomBlocks
  validateReferences rooms items
  return (rooms, items)

-- Divide el contenido en bloques separados por "---"
splitBlocks :: String -> [String]
splitBlocks content =
  let rawBlocks = splitOn "---" content
  in filter (not . null . filter (not . null) . lines) rawBlocks

-- Función auxiliar para dividir strings
splitOn :: String -> String -> [String]
splitOn delimiter str = go str []
  where
    go [] acc = [reverse acc]
    go s acc =
      case stripPrefix delimiter s of
        Just rest -> reverse acc : go rest []
        Nothing -> go (tail s) (head s : acc)

-- Separa bloques de ITEM y SALA
partitionBlocks :: [String] -> ([String], [String])
partitionBlocks blocks = (itemBlocks, roomBlocks)
  where
    itemBlocks = filter isItemBlock blocks
    roomBlocks = filter isRoomBlock blocks
    isItemBlock block = "ITEM:" `elem` map (take 5) (lines block)
    isRoomBlock block = "SALA:" `elem` map (take 5) (lines block)

-- Parsea todos los bloques de items
parseItems :: [String] -> Either String ItemContainer
parseItems blocks = do
  items <- mapM parseItemBlock blocks
  return $ Map.fromList items

-- Parsea un bloque de item individual
parseItemBlock :: String -> Either String (String, Item)
parseItemBlock block = do
  let lns = filter (not . null) $ lines block
  itemNameLine <- findLine "ITEM:" lns
  itemDescLine <- findLine "DESC:" lns
  let name = trim $ drop 5 itemNameLine
  let desc = trim $ drop 5 itemDescLine
  if null name
    then Left "Item sin nombre"
    else return (name, Item name desc)

-- Parsea todos los bloques de salas
parseRooms :: [String] -> Either String RoomContainer
parseRooms blocks = do
  rooms <- mapM parseRoomBlock blocks
  return $ Map.fromList rooms

-- Parsea un bloque de sala individual
parseRoomBlock :: String -> Either String (String, Room)
parseRoomBlock block = do
  let lns = filter (not . null) $ lines block
  roomNameLine <- findLine "SALA:" lns
  roomDescLine <- findLine "DESC:" lns
  let name = trim $ drop 5 roomNameLine
  let desc = trim $ drop 5 roomDescLine
  let exitLines = filter (\l -> take 7 l == "SALIDA:") lns
  let objectLines = filter (\l -> take 7 l == "OBJETO:") lns
  exits <- parseExits exitLines
  let objects = map (trim . drop 7) objectLines
  if null name
    then Left "Sala sin nombre"
    else return (name, Room name desc exits objects)

-- Parsea las salidas de una sala
parseExits :: [String] -> Either String (Map.Map Direction String)
parseExits exitLines = do
  exits <- mapM parseExitLine exitLines
  return $ Map.fromList exits

-- Parsea una línea de salida: "SALIDA: Norte -> Cocina"
parseExitLine :: String -> Either String (Direction, String)
parseExitLine line = do
  let content = trim $ drop 7 line  -- Quitar "SALIDA:"
  case break (== '-') content of
    (dirStr, '-':'>':roomStr) -> do
      dir <- parseDirection (trim dirStr)
      return (dir, trim roomStr)
    _ -> Left $ "Formato de salida inválido: " ++ line

-- Parsea una dirección desde un string
parseDirection :: String -> Either String Direction
parseDirection dirStr =
  case map toLower dirStr of
    "norte" -> Right Norte
    "sur" -> Right Sur
    "este" -> Right Este
    "oeste" -> Right Oeste
    _ -> Left $ "Dirección desconocida: " ++ dirStr

-- Busca una línea que comience con un prefijo
findLine :: String -> [String] -> Either String String
findLine prefix lines =
  case filter (\l -> take (length prefix) l == prefix) lines of
    (x:_) -> Right x
    [] -> Left $ "No se encontró línea con prefijo: " ++ prefix

-- Elimina espacios al inicio y final
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")

-- Valida que todos los objetos referenciados en salas existen
validateReferences :: RoomContainer -> ItemContainer -> Either String ()
validateReferences rooms items = do
  let allObjectRefs = concatMap roomObjects (Map.elems rooms)
  let missingObjects = filter (`Map.notMember` items) allObjectRefs
  if null missingObjects
    then Right ()
    else Left $ "Objetos referenciados pero no definidos: " ++ show missingObjects