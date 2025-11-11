module Engine.Persistence (loadWorldData) where

import Control.Monad (foldM)
import Engine.Types
import qualified Data.Map as Map
import Data.List (stripPrefix)
import Data.Char (isSpace, toLower)
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
  normalizedRooms <- normalizeRooms rooms items
  return (normalizedRooms, items)

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

-- Separa bloques de ITEM y SALA (insensible a mayúsculas/minúsculas)
partitionBlocks :: [String] -> ([String], [String])
partitionBlocks blocks = (itemBlocks, roomBlocks)
  where
    itemBlocks = filter (containsPrefix "ITEM:") blocks
    roomBlocks = filter (containsPrefix "SALA:") blocks
    containsPrefix prefix block =
      any (startsWithCI prefix) (map trimStart (lines block))

-- Parsea todos los bloques de items
parseItems :: [String] -> Either String ItemContainer
parseItems blocks = do
  items <- mapM parseItemBlock blocks
  return $ Map.fromList items

-- Parsea un bloque de item individual
parseItemBlock :: String -> Either String (String, Item)
parseItemBlock block = do
  let lns = filter (not . null . trimStart) $ lines block
  name <- findField "ITEM:" lns
  desc <- findField "DESC:" lns
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
  let lns = filter (not . null . trimStart) $ lines block
  name <- findField "SALA:" lns
  desc <- findField "DESC:" lns
  let exitLines = filter (startsWithCI "SALIDA:") lns
  let objectNames = mapMaybe (stripField "OBJETO:") lns
  exits <- parseExits exitLines
  if null name
    then Left "Sala sin nombre"
    else return (name, Room name desc exits objectNames)

-- Parsea las salidas de una sala
parseExits :: [String] -> Either String (Map.Map Direction String)
parseExits exitLines = do
  exits <- mapM parseExitLine exitLines
  return $ Map.fromList exits

-- Parsea una línea de salida: "SALIDA: Norte -> Cocina"
parseExitLine :: String -> Either String (Direction, String)
parseExitLine line = do
  content <- case dropPrefixCI "SALIDA:" line of
    Just rest -> Right (trim rest)
    Nothing -> Left $ "Formato de salida inválido: " ++ line
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

-- Busca una línea que comience con un prefijo y devuelve su valor
findField :: String -> [String] -> Either String String
findField prefix lines =
  case mapMaybe (stripField prefix) lines of
    (x:_) -> Right x
    [] -> Left $ "No se encontró línea con prefijo: " ++ prefix

stripField :: String -> String -> Maybe String
stripField prefix line =
  fmap trim (dropPrefixCI prefix line)

dropPrefixCI :: String -> String -> Maybe String
dropPrefixCI prefix line =
  let cleaned = trimStart line
      len = length prefix
  in if length cleaned >= len &&
        map toLower (take len cleaned) == map toLower prefix
        then Just (drop len cleaned)
        else Nothing

startsWithCI :: String -> String -> Bool
startsWithCI prefix line =
  case dropPrefixCI prefix line of
    Just _ -> True
    Nothing -> False

trimStart :: String -> String
trimStart = dropWhile isSpace

-- Elimina espacios al inicio y final
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")

-- Normaliza referencias de salas y objetos, haciendo que los nombres coincidan con los definidos
normalizeRooms :: RoomContainer -> ItemContainer -> Either String RoomContainer
normalizeRooms rooms items = do
  roomCanon <- buildCanonicalMap "salas" (Map.keys rooms)
  itemCanon <- buildCanonicalMap "objetos" (Map.keys items)
  Map.traverseWithKey (normalizeRoom roomCanon itemCanon) rooms

normalizeRoom :: Map.Map String String -> Map.Map String String -> String -> Room -> Either String Room
normalizeRoom roomCanon itemCanon roomName room = do
  exits' <- traverseExits (roomExits room)
  objects' <- mapM resolveObject (roomObjects room)
  return room { roomExits = exits', roomObjects = objects' }
  where
    traverseExits = Map.traverseWithKey resolveRoom

    resolveRoom dir dest =
      case Map.lookup (normalizeKey dest) roomCanon of
        Just actual -> Right actual
        Nothing ->
          Left $
            "La sala '" ++ roomName ++ "' tiene una salida (" ++ show dir ++ ") hacia '"
            ++ dest ++ "' que no coincide con ninguna sala definida."

    resolveObject objName =
      case Map.lookup (normalizeKey objName) itemCanon of
        Just actual -> Right actual
        Nothing -> Left $ "La sala '" ++ roomName ++ "' referencia el objeto '" ++ objName ++ "' que no existe."

-- Construye un mapa de nombres canónicos evitando duplicados insensibles a mayúsculas/minúsculas
buildCanonicalMap :: String -> [String] -> Either String (Map.Map String String)
buildCanonicalMap label names =
  foldM insertCanon Map.empty names
  where
    insertCanon acc name =
      let key = normalizeKey name
      in case Map.lookup key acc of
           Nothing -> Right (Map.insert key name acc)
           Just existing ->
             Left $
               "Nombres de " ++ label ++ " duplicados que difieren solo por mayúsculas/minúsculas: "
               ++ show existing ++ " y " ++ show name

normalizeKey :: String -> String
normalizeKey = map toLower . trim
