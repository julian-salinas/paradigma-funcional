import Text.Show.Functions

-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = Tiro 10 ((*2) . precisionJugador $ habilidad) 0

madera :: Palo
madera habilidad = Tiro 100 (flip div 2 . precisionJugador $ habilidad) 5

hierro :: Int -> Palo
hierro numero habilidad = Tiro ((*numero) . fuerzaJugador $ habilidad) (flip div 2 . precisionJugador $ habilidad) (numero - 3)

palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

golpe :: Palo -> Jugador -> Tiro
golpe palo jugador = palo . habilidad $ jugador

type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita tiro 
  |(precisionMayorA 90 tiro) && (estaAlRasDelSuelo $ tiro) = duplicarVelocidad . mapPrecision 100 . mapAltura 0 $ tiro 
  |otherwise                                     = anularTiro tiro
  where precisionMayorA = atributoMayorA precision

laguna :: Int -> Obstaculo
laguna largoLaguna tiro
  |(velocidadMayorA 80 $ tiro) && (between 1 5 . altura $ tiro) = mapAltura (div (altura tiro) largoLaguna) $ tiro
  |otherwise                                                    = anularTiro tiro
  where velocidadMayorA = atributoMayorA velocidad

hoyo :: Obstaculo
hoyo tiro
  |(between 5 20 . velocidad $ tiro) && (estaAlRasDelSuelo tiro) = anularTiro tiro
  |otherwise                                                     = anularTiro tiro

anularTiro :: Tiro -> Tiro
anularTiro (Tiro _ _ _) = Tiro 0 0 0

mapPrecision :: Int -> Tiro -> Tiro
mapPrecision numero tiro = tiro {precision = numero}

mapAltura :: Int -> Tiro -> Tiro
mapAltura numero tiro = tiro {altura = numero}

mapVelocidad :: Int -> Tiro -> Tiro
mapVelocidad numero tiro = tiro {velocidad = numero}

atributoMayorA :: (Tiro -> Int) -> Int -> Tiro -> Bool
atributoMayorA atributo numero tiro = (>numero) . atributo $ tiro

--precisionMayorA :: Int -> Tiro -> Bool
--precisionMayorA numero tiro = (>numero) . precision $ tiro
--
--velocidadMayorA :: Int -> Tiro -> Bool
--velocidadMayorA numero tiro = (>numero) . velocidad $ tiro

estaAlRasDelSuelo :: Tiro -> Bool
estaAlRasDelSuelo tiro = (==0) . altura $ tiro

duplicarVelocidad :: Tiro -> Tiro
duplicarVelocidad tiro = tiro {velocidad = (*2) . velocidad $ tiro}

palosUtiles :: [Palo] -> Obstaculo -> Jugador -> [Palo]
palosUtiles [] _ _ = []
palosUtiles (cabeza:cola) obstaculo jugador
  |sirvePalo (cabeza) obstaculo jugador = cabeza : palosUtiles (cola) obstaculo jugador
  |otherwise                            = palosUtiles (cola) obstaculo jugador

sirvePalo :: Palo -> Obstaculo -> Jugador -> Bool
sirvePalo palo obstaculo jugador = tiroSobrevivio obstaculo . (golpe palo) $ jugador

estaDetenido :: Tiro -> Bool
estaDetenido tiro = (==0) . (+ (velocidad tiro)) . (+ (altura tiro)) . precision $ tiro

cuantosPuedeSuperar :: [Obstaculo] -> Tiro -> Int
cuantosPuedeSuperar obstaculos tiro = foldl (sumarSiSupera tiro) 0 obstaculos

cuantosPuedeSuperar' :: [Obstaculo] -> Tiro -> Int
cuantosPuedeSuperar' obstaculos tiro = length (takeWhile (tiroSobrevivio' tiro) obstaculos)

sumarSiSupera :: Tiro -> Int -> Obstaculo -> Int
sumarSiSupera tiro numero obstaculo
  |tiroSobrevivio obstaculo tiro = numero + 1
  |otherwise                     = numero

tiroSobrevivio' :: Tiro -> Obstaculo -> Bool
tiroSobrevivio' tiro obstaculo = not . estaDetenido . obstaculo $ tiro

tiroSobrevivio :: Obstaculo -> Tiro -> Bool
tiroSobrevivio obstaculo tiro = not . estaDetenido . obstaculo $ tiro 

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (cuantosPuedeSuperar' obstaculos . flip golpe jugador) palos

apuesta :: [(Jugador,Puntos)]
apuesta = [(bart, 50),(todd, 100),(rafa, 70)]

type Jugadores = [(Jugador,Puntos)]

resultadoApuesta :: Jugadores -> [String]
resultadoApuesta jugadores = map (padre . fst) (listaPerdedores jugadores)

listaPerdedores :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
listaPerdedores jugadores = filter (esPerdedor jugadores) jugadores

esPerdedor :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
esPerdedor jugadores jugador = any (puedeGanarle jugador) jugadores

puedeGanarle :: (Jugador, Puntos) -> (Jugador, Puntos) -> Bool
puedeGanarle unJugador otroJugador = snd unJugador < snd otroJugador