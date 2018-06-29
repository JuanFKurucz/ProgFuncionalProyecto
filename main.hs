import Data.List
type Fila = [TablutCasillero]
type Columna = [Fila]
type Tablero = Columna
type Posicion = (Int, Int)
type NumeroFila = Int
type NumeroColumna = Int
type TablutAction = (Posicion,Posicion)--donde la primera posicion es la inicial (donde estoy) y la segunda es la final (a la que quiero llegar).

--data Pieza = PeonNegro SwordPlayer| PeonBlanco ShieldPlayer | Rey ShieldPlayer deriving (Eq, Show)
data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show, Enum)
data TablutCasillero = PeonNegro| PeonBlanco | Rey | Empty deriving (Eq)
data TablutGame = TG Tablero TablutPlayer deriving (Show)

instance Show TablutCasillero where
	show (PeonNegro) = show "N"
	show (PeonBlanco) = show "B"
	show (Rey) = show "R"
	show (Empty) = show " "

beginning :: TablutGame
beginning = (TG game ShieldPlayer)
	where
	game=[fila1, fila2, fila3, fila4, fila5, fila4, fila3, fila2, fila1]
	fila1 = [Empty, Empty, Empty, (PeonNegro), (PeonNegro), (PeonNegro), Empty, Empty, Empty]
	fila2 = [Empty, Empty, Empty, Empty, (PeonNegro), Empty, Empty, Empty, Empty]
	fila3 = [Empty, Empty, Empty, Empty, (PeonBlanco), Empty, Empty, Empty, Empty]
	fila4 = [(PeonNegro), Empty, Empty, Empty, (PeonBlanco), Empty, Empty, Empty, (PeonNegro)]
	fila5 = [(PeonNegro), (PeonNegro), (PeonBlanco), (PeonBlanco), (Rey), (PeonBlanco), (PeonBlanco), (PeonNegro), (PeonNegro)]

showGame :: TablutGame -> String
showGame (TG t p) = aux (t)

aux :: Tablero -> String
aux (x:xs) = aux2 x ++ "\n" ++ aux xs
aux [] = ""

aux2 :: Fila -> String
aux2 (x:xs) = show x ++ " " ++ aux2 xs
aux2 [] = ""

isValidPosition::  Posicion -> Bool --Esta funci'on toma un tablero y una posici'on y me dice si est'a dentro del tablero
isValidPosition (x, y) = x >=0 && x <=8 && y>=0 && y<=8

isEmpty :: Tablero -> Posicion -> Bool
isEmpty tab (x,y) = (tab !! x) !! y == Empty --me dan un tablero y una posicion, busca en el tablero si ese casillero esta vacio y devuelve verdadero en ese caso

armarParesFila :: TablutCasillero -> NumeroFila -> Fila -> [(NumeroFila,NumeroColumna)]
armarParesFila casillero x filaEntera = map (\y -> (x,y)) (findIndices (==casillero ) filaEntera) --findindices se encarga de dado una fila y un predicado devuelve los indices de los elementos que cumplan el predicado. ejemplo tengo una ficha blanca en una fila y me devuelve la posicion x y de esa ficha blanca en esa fila

movimientosArriba :: Tablero -> Posicion -> [Posicion] --esta funcion recibe un tablero y la posicion de una ficha y me dice que movimientos hacia arriba puede hacer en una lista de posibles posiciones
movimientosArriba table (x,y) = if (y -1) < 0 then [] else takeWhile (isEmpty table) (map (\posY -> (x,posY))[0..(y-1)])
--el y-1 hasta 0 representa en una columna, estando en una posicion y, las posiciones posibles hacia arriba hasta 0
--el map le agrega el numero de la fila a todas las posiciones "y"
--el takewhile se queda con todas las posiciones que son empty hasta encontrarse con una ficha, ahi para.

movimientosAbajo :: Tablero -> Posicion -> [Posicion]
movimientosAbajo tablet (x,y) = if (y+1) > 8 then [] else takeWhile (isEmpty tablet) (map (\posY -> (x,posY))[(y+1)..8])

movimientosDerecha :: Tablero -> Posicion -> [Posicion]
movimientosDerecha  tablett (x,y) = if (x+1) > 8 then [] else takeWhile (isEmpty tablett) (map (\posX -> (posX,y))[(x+1)..8])

movimientosIzquierda :: Tablero -> Posicion -> [Posicion]
movimientosIzquierda tablettt (x,y) = if (x-1) < 0 then [] else takeWhile (isEmpty tablettt) (map (\posX -> (posX,y))[0..(x-1)])

todosLosMovimientos :: Tablero -> Posicion ->[Posicion]
todosLosMovimientos tabb pos  = movimientosIzquierda tabb pos ++ movimientosDerecha tabb pos ++ movimientosArriba tabb pos ++ movimientosAbajo tabb pos

asociarMovimientos :: Posicion ->[Posicion] -> [TablutAction]
asociarMovimientos inicial finales = map (\final -> (inicial,final)) (finales)

--actions tiene que:
--1) ver de quien es el turno
--2) conseguir las posiciones de las fichas de ese jugador
--3) una vez que tengo las fichas, armar la lista de posiciones posibles para cada ficha.
--4) convertir esa lista en una lista de acciones
--5) devolver la lista con los 2 jugadores  sus acciones posibles

actions :: TablutGame -> [(TablutPlayer, [TablutAction])]
actions  (TG tableroo ShieldPlayer) = [(ShieldPlayer, concat (map (\posFicha -> asociarMovimientos posFicha (todosLosMovimientos tableroo posFicha))(concat (map (\numFila -> armarParesFila PeonBlanco numFila (tableroo !! numFila) ) [0..8])))), (SwordPlayer, [])]
actions  (TG tableroo SwordPlayer) = [(SwordPlayer, concat (map (\posFicha -> asociarMovimientos posFicha (todosLosMovimientos tableroo posFicha))(concat (map (\numFila -> armarParesFila PeonNegro numFila (tableroo !! numFila) ) [0..8])))), (ShieldPlayer, [])]

next :: TablutGame -> (TablutPlayer, TablutAction) -> TablutGame
next (TG tableroo player) (actionPlayer, action)
	| player /= acitonPlayer = error "No es el turno de este jugador"
	| otherwise = (TablutGame tablerooMod nextPlayer)
	where
		nextPlayer = if player == SwordPlayer then ShieldPlayer else SwordPlayer
		tablerooMod =
