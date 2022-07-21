type Sentencia = (Tablero -> Tablero)
type Posicion = (Int, Int)
type Dimension = (Int, Int)
type Condicion = (Tablero -> Bool)

posicionInicial :: Posicion
posicionInicial = (1,1)

data Tablero = Tablero{
    tamanio :: (Int,Int),
    celdas :: [Celda],
    cabezal :: Posicion
}deriving(Show)

data Celda = Celda{
    posicion :: Posicion,
    bolitas :: [Bolita]
}deriving(Show)

data Bolita = Negro | Rojo | Azul | Verde deriving(Show,Eq)

data Direccion = Norte | Sur | Este | Oeste deriving(Show,Eq)

data Grupo = Fila | Columna deriving(Show,Eq)

inicializarTablero :: Dimension -> Tablero
inicializarTablero dimension = Tablero dimension (inicializarCeldas posicionInicial dimension []) posicionInicial

mover :: Direccion -> Sentencia
mover Norte tablero = correr Fila 1 tablero
mover Sur tablero = correr Fila (-1) tablero
mover Este tablero = correr Columna 1 tablero
mover Oeste tablero = correr Columna (-1) tablero

correr :: Grupo -> Int -> Sentencia
correr Fila numero (Tablero tamanio celdas (x,y)) = Tablero tamanio celdas (x + numero, y)
correr Columna numero (Tablero tamanio celdas (x,y)) = Tablero tamanio celdas (x, y + numero)

inicializarCeldas :: Posicion -> (Int,Int) -> [Celda] -> [Celda]
inicializarCeldas (x, y) (filas,columnas) celdas
  |hayQueAgregarColumna (x,y) (filas,columnas) = inicializarCeldas (siguienteColumna (x,y)) (filas,columnas) (agregarUnaCeldaVaciaEn celdas (x, y))
  |hayQueAgregarFila (x,y) (filas,columnas) = inicializarCeldas (siguienteFila (x,y)) (filas,columnas) (agregarUnaCeldaVaciaEn celdas (x, y))
  |otherwise = (agregarUnaCeldaVaciaEn celdas (x, y))

siguienteColumna :: Posicion -> Posicion
siguienteColumna (x,y) = (x, y + 1)

siguienteFila :: Posicion -> Posicion
siguienteFila (x,y) = (x + 1, 1)

hayQueAgregarFila :: Posicion -> Posicion -> Bool
hayQueAgregarFila (x,y) (filas,columnas) = x < filas

hayQueAgregarColumna :: Posicion -> Posicion -> Bool
hayQueAgregarColumna (x,y) (filas,columnas) = y < columnas

agregarUnaCeldaVaciaEn :: [Celda] -> Posicion -> [Celda]
agregarUnaCeldaVaciaEn celdas (x, y) = celdas ++ [Celda (x, y) []]

aplicarSentencias :: [Sentencia] -> Sentencia
aplicarSentencias sentencias tablero = foldl (flip ($)) tablero sentencias

repetir :: Int -> [Sentencia] -> Sentencia
repetir 0 _ tablero = tablero
repetir n sentencias tablero = repetir (n - 1) sentencias (aplicarSentencias sentencias tablero)

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion sentenciasSi sentenciasSino tablero
  |condicion tablero = aplicarSentencias sentenciasSi tablero
  |otherwise         = aplicarSentencias sentenciasSino tablero

sentenciaNula :: Sentencia
sentenciaNula = id

si :: Condicion -> [Sentencia] -> Tablero -> Tablero
si condicion sentencias tablero = alternativa condicion sentencias [sentenciaNula] tablero

siNo :: Condicion -> [Sentencia] -> Tablero -> Tablero
siNo condicion sentencias tablero = alternativa condicion [sentenciaNula] sentencias tablero

mapCeldas :: (Posicion -> Bolita -> Celda -> Celda) -> Bolita -> Sentencia  
mapCeldas funcion bolita tablero = tablero {celdas = map (funcion (cabezal tablero) bolita) $ celdas tablero}

poner :: Bolita -> Tablero -> Tablero
poner = mapCeldas ponerSi

sacar :: Bolita -> Tablero -> Tablero
sacar = mapCeldas sacarSi

data Accion = Poner | Sacar deriving (Show,Eq)

operarSi :: Posicion -> Bolita -> Celda -> Accion -> Celda
operarSi cabezal bolita celda accion
  |celdaEsCabezal celda cabezal = celda {bolitas = modificarBolitas accion (bolitas celda) bolita}
  |otherwise = celda

ponerSi :: Posicion -> Bolita -> Celda -> Celda
ponerSi cabezal bolita celda = operarSi cabezal bolita celda Poner

sacarSi :: Posicion -> Bolita -> Celda -> Celda
sacarSi cabezal bolita celda = operarSi cabezal bolita celda Sacar

modificarBolitas :: Accion -> [Bolita] -> Bolita -> [Bolita]
modificarBolitas Poner bolitas bolita = (bolita:bolitas)
modificarBolitas Sacar bolitas bolita = filter (/= bolita) bolitas ++ (tail . filter (== bolita) $ bolitas)

irAlBorde :: Direccion -> Sentencia
irAlBorde direccion tablero = mientras (puedeMoverse direccion) [mover direccion] tablero

puedeMoverse :: Direccion -> Tablero -> Bool
puedeMoverse direccion tablero = permaneceAdentro (cabezal .mover direccion $ tablero) (tamanio tablero)

permaneceAdentro :: Posicion -> Posicion -> Bool
permaneceAdentro (x,y) (a,b) = x >= 1 && x <= a && y >= 1 && y <= b 

--uso funciones filas y columnas para ganar algo de expresividad, en lugar de fst y snd
filas :: (Int,Int) -> Int
filas = fst

columnas :: (Int,Int) -> Int
columnas = snd

hayBolita :: Bolita -> Tablero -> Bool
hayBolita bolita tablero = any (bolitaEnCelda (cabezal tablero) bolita) $ celdas tablero

bolitaEnCelda :: Posicion -> Bolita -> Celda -> Bool
bolitaEnCelda cabezal bolita celda = elem bolita (bolitas celda) && (celdaEsCabezal celda cabezal)

cantidadDeBolitas :: Bolita -> Tablero -> Int
cantidadDeBolitas bolita = length . filtrarPorColor bolita . bolitas . agarrarCeldaCabezal

agarrarCeldaCabezal :: Tablero -> Celda
agarrarCeldaCabezal tablero = head . filter ((== cabezal tablero) . posicion) . celdas $ tablero

filtrarPorColor :: Bolita -> [Bolita] -> [Bolita]
filtrarPorColor bolita = filter (==bolita)

celdaEsCabezal :: Celda -> Posicion -> Bool
celdaEsCabezal celda cabezal = posicion celda == cabezal

mientras :: Condicion -> [Sentencia] -> Sentencia
mientras condicion sentencias tablero
  |condicion tablero = mientras condicion sentencias (aplicarSentencias sentencias tablero)
  |otherwise         = tablero

programa :: [Sentencia] -> Tablero -> Tablero
programa = aplicarSentencias

tableroVacio :: Tablero
tableroVacio = inicializarTablero (3, 3)
