# Proyecto 1: Motor de Aventura de Texto (CI-3661)

- **Nombres:** (Eliezer Cario,Angel Rodriguez)
- **Carnets:** (18-10605, 15-11669)

---

## Cómo Compilar y Ejecutar

Este proyecto usa `stack`. Para compilarlo y ejecutarlo:

1.  **Compilar:** `stack build`
2.  **Ejecutar:** `stack exec TextAdventureEngine-exe`

---

## Justificación de Diseño

### 1. Elección de Estructuras de Datos

#### Uso de `Data.Map` para el Mundo y las Salidas

Hemos elegido usar `Map String Room` para representar el mundo completo de salas, `Map String Item` para los items globales, y `Map Direction String` para las salidas de cada sala. Las razones principales son:

**Ventajas de usar Map:**
- **Búsqueda eficiente O(log n)**: Cuando el jugador se mueve a una sala o intenta tomar un objeto, necesitamos buscar por nombre. Con Map podemos realizar búsquedas con complejidad logarítmica, la cual es más eficientes que las búsquedas lineales O(n) en listas.
- **Claves únicas garantizadas**: Map automáticamente garantiza que no habrá salas u objetos duplicados con el mismo nombre, lo cual es un requisito del dominio.
- **Actualizaciones eficientes**: Cuando un jugador toma un objeto de una sala, necesitamos actualizar el estado de esa sala. Con Map, podemos hacer `Map.insert` de forma eficiente sin tener que reconstruir toda la estructura.
- **Semántica clara**: El uso de Map expresa claramente la intención de que estamos indexando por nombres únicos (salas, objetos).

**Desventajas consideradas:**
- Requiere importar el módulo `Data.Map`.
- Es ligeramente más complejo para iterar sobre todos los elementos (aunque esto es raro en nuestro juego).

**Alternativa descartada (Listas):**
Si hubiéramos usado listas, cada búsqueda de sala u objeto sería O(n), y tendríamos que validar manualmente la unicidad de nombres. Para un motor extensible, la eficiencia de Map es preferible.

#### Uso de Listas para Objetos en Salas

Para `roomObjects :: [String]` en el registro `Room`, elegimos una lista simple de nombres de objetos:

**Razones:**
- **Orden puede ser relevante**: El orden en que se listan los objetos podría ser importante para la narrativa (ej. mostrar objetos importantes primero).
- **Simplicidad**: Los objetos en una sala individual son típicamente pocos (1-5), por lo que O(n) para búsqueda es aceptable.
- **Modificación sencilla**: Remover un objeto es una operación simple con `filter`.

### 2. Separación de Lógica Pura e Impura

Este diseño logra una separación estricta entre lógica pura y efectos de I/O:

#### Módulos Puros (sin IO)

**Engine.Types**:
- Define únicamente tipos de datos algebraicos.
- No contiene funciones, solo definiciones de tipos.
- Completamente puro.

**Engine.Parser**:
- `parseCommand :: String -> Maybe Command`
- Función pura que transforma strings a comandos.
- No realiza I/O, solo computación determinística.

**Engine.Core**:
- `processCommand :: Command -> GameState -> (String, GameState)`
- **El corazón del motor, completamente puro**.
- Toma un comando y un estado, devuelve un mensaje y un nuevo estado.
- No hay `IO` en ninguna de sus funciones.
- Toda la lógica del juego (mover, tomar objetos, validaciones) está aquí, sin efectos de borde.
- Esta función es **testeable sin I/O**: podemos probarla con estados de entrada y verificar estados de salida sin ejecutar el juego.

#### Módulos con I/O (impuros)

**Engine.Persistence**:
- `loadWorldData :: FilePath -> IO (Either String (...))`
- Único módulo de Engine que hace I/O (lectura de archivo).
- El parseo del contenido del archivo es puro internamente, pero la lectura requiere IO.
- Devuelve `Either` para manejar errores de parseo de forma funcional.

**Main.hs**:
- Contiene toda la interacción con el usuario (I/O).
- `main :: IO ()`: Lee el archivo, maneja errores de carga, inicia el juego.
- `gameLoop :: GameState -> IO ()`: Lee entrada del usuario, imprime salida.
- **Delega toda la lógica del juego a `Engine.Core.processCommand`**.
- Main.hs solo se encarga de:
  1. Leer líneas de entrada
  2. Llamar a `parseCommand` (pura)
  3. Llamar a `processCommand` (pura)
  4. Imprimir el resultado
  5. Recursión del bucle

#### Flujo de Datos

```
[Usuario escribe comando] (IO)
    ↓
[Main.hs: getLine] (IO)
    ↓
[Engine.Parser.parseCommand] (PURO)
    ↓
[Engine.Core.processCommand] (PURO)
    ↓
[Main.hs: putStrLn resultado] (IO)
```

#### Beneficios de esta Separación

1. **Testabilidad**: Podemos probar `processCommand` con casos de prueba sin ejecutar I/O.
2. **Razonamiento**: La lógica pura es más fácil de entender porque no tiene efectos ocultos.
3. **Reutilización**: El motor puro (`Engine.Core`) podría usarse con diferentes interfaces (CLI, web, GUI) solo cambiando Main.hs.
4. **Mantenibilidad**: Los cambios en la lógica del juego no afectan el I/O y viceversa.

### 3. Sala Inicial Aleatoria
El juego, inicialmente, comienza la partida eligiendo la sala por orden alfabetico, dejar una sala fija, traería problemas al pasar diferentes archivos 'mundo.txt', por tanto se tomo la decisión de iniciar la partida en una sala seleccionada aleatoriamente entre todas las salas disponibles. Esto se implementó usando System.Random para elegir un índice aleatorio de la lista de salas cargadas. Esto añade variedad y rejugabilidad al juego.

Comandos Disponibles
El motor soporta los siguientes comandos:
ComandoDescripciónir <dirección>Mueve al jugador a otra sala. Direcciones válidas: 'norte, sur, este, oeste'. 'mirar' Muestra la descripción de la sala actual. 'tomar <objeto>' Toma un objeto de la sala y lo añade al inventario. 'coger <objeto>' Sinónimo de tomar. 'inventario' o 'inv' Muestra los objetos en el inventario. 'salir' Termina el juego.

Pruebas Unitarias
El proyecto incluye pruebas unitarias para garantizar que la lógica del juego funcione correctamente. Las pruebas cubren:

Parseo de comandos: Verificar que los comandos se interpreten correctamente.
Lógica del juego: Verificar que los comandos se procesen correctamente y que el estado del juego se actualice como se espera.
Manejo de errores: Verificar que los mensajes de error sean claros y útiles.

las pruebas cubren casos límite como lo es (tomar un objeto inexistente, ir a una dirección no disponible)

El diseño modular, permite añadir caracteristicas en el futuro sin cambiar la arquitectura existente, por ejemplo: implementar 'dejar'

## Ejecutar pruebas con:
**stack test**
