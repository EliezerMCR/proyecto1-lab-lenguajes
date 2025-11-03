module Engine.Persistence (loadWorldData) where

import Engine.Types

-- Carga el archivo del mundo.
-- Devuelve (IO (Either Error (MapaDeSalas, MapaDeItems)))
loadWorldData :: FilePath -> IO (Either String (RoomContainer, ItemContainer))
loadWorldData filePath = do
  -- Pista: usa 'readFile' para leer el archivo.
  -- Luego, parsea el contenido.
  -- Si el parseo falla, devuelve (Left "Mensaje de Error")
  -- Si tiene éxito, devuelve (Right (mapaSalas, mapaItems))
  undefined