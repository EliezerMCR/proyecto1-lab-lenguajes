module Engine.Parser (parseCommand) where

import Engine.Types
import Data.Char (toLower)

-- Parsea la entrada del usuario (String) a un (Maybe Command)
parseCommand :: String -> Maybe Command
parseCommand input =
  let normalized = map toLower (trim input)
      ws = words normalized
  in case ws of
    [] -> Nothing

    -- Comando: ir <direccion>
    ["ir", dir] -> Ir <$> parseDir dir

    -- Comando: mirar
    ["mirar"] -> Just Mirar

    -- Comando: tomar <objeto>
    ("tomar":rest) ->
      if null rest
        then Nothing
        else Just (Tomar (unwords rest))

    -- Comando: coger <objeto>
    ("coger":rest) ->
      if null rest
        then Nothing
        else Just (Coger (unwords rest))

    -- Comando: inventario
    ["inventario"] -> Just Inventario

    -- Comando: inv (abreviación)
    ["inv"] -> Just Inv

    -- Comando: salir
    ["salir"] -> Just Salir

    -- Comando no reconocido
    _ -> Nothing

-- Parsea una dirección desde un string
parseDir :: String -> Maybe Direction
parseDir dir =
  case dir of
    "norte" -> Just Norte
    "sur" -> Just Sur
    "este" -> Just Este
    "oeste" -> Just Oeste
    _ -> Nothing

-- Elimina espacios al inicio y final
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")