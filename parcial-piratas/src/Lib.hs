data Tripulante = Tripulante{
    --nombre :: String,
    energia :: Int
}deriving (Show,Eq)

mapEnergia :: Int -> Tripulante -> Tripulante
mapEnergia valor tripulante = tripulante{energia = energia tripulante + valor}

enfrentarEsqueleto :: Tripulante -> Tripulante
enfrentarEsqueleto tripulante
  |estaEnLasUltimas tripulante = mapEnergia (-20) tripulante
  |otherwise                   = mapEnergia (-10) tripulante

estaEnLasUltimas :: Tripulante -> Bool
estaEnLasUltimas = (<50) . energia

transportarCarga :: Int -> Tripulante -> Tripulante
transportarCarga peso = mapEnergia (-peso)

beberGrog :: Tripulante -> Tripulante
beberGrog = mapEnergia 20

estaMuerto :: Tripulante -> Bool
estaMuerto = (==0) . energia

data Barco = Barco{
    tamanio :: Int,
    tripulacion :: [Tripulante],
    oro :: Int,
    madera :: Int,
    balasDeCanion :: Int,
    balas :: Int
}deriving (Show,Eq)

juan = Tripulante 30

pedro = Tripulante 0

tripulantesPrueba = [juan,pedro]

barcoPrueba = Barco {tamanio = 150, tripulacion = [], oro = 0, madera = 0, balasDeCanion = 0, balas = 50}

esBarcoFantasma :: Barco -> Bool
esBarcoFantasma = (==0) . length . tripulacion

llenarBarco :: [Tripulante] -> Barco -> Barco
llenarBarco tripulantes = llenarBarcoDeOro . llenarBarcoDeTripulantes tripulantes

llenarBarcoDeOro :: Barco -> Barco
llenarBarcoDeOro barco
  |entraMasOro barco = llenarBarcoDeOro (mapOro 1 barco)
  |otherwise   = barco

llenarBarcoDeOro' :: Barco -> Barco
llenarBarcoDeOro' barco = barco {oro = div (tamanio barco) 7}

llenarBarcoDeTripulantes :: [Tripulante] -> Barco -> Barco
llenarBarcoDeTripulantes [] barco = barco
llenarBarcoDeTripulantes (unTripulante : restoTripulantes) barco
  |hayLugarParaUnoMas barco = llenarBarcoDeTripulantes restoTripulantes (agregarTripulante unTripulante barco)
  |otherwise          = barco

entraMasOro :: Barco -> Bool
entraMasOro barco
  |oro barco == 0 = True
  |otherwise = (>= 7) . div (tamanio barco) . oro $ barco

type ModificarBarco = Barco -> Barco

mapOro :: Int -> ModificarBarco
mapOro valor barco = barco {oro = oro barco + valor}

hayLugarParaUnoMas :: Barco -> Bool
hayLugarParaUnoMas barco = (>=3) . div (balas barco) . length . tripulacion $ barco

agregarTripulante :: Tripulante -> Barco -> Barco
agregarTripulante tripulante barco = barco{tripulacion = tripulante : tripulacion barco}

enfrentamiento :: Barco -> Barco -> Barco
enfrentamiento unBarco otroBarco
  |tieneMas tamanio unBarco otroBarco && (tieneMas balas unBarco otroBarco)                   = ganar
  |tieneMas tamanio otroBarco unBarco && tieneMas cantidadTripulantesVivos unBarco otroBarco  = ganar
  |tieneMas madera unBarco otroBarco                                                          = ganar
  |otherwise                                                                                  = vaciarRecursos unBarco
  where ganar = robarseLosRecursos unBarco otroBarco

cantidadTripulantesVivos :: Barco -> Int
cantidadTripulantesVivos = (length . filter estaVivo . tripulacion)

tieneMas :: (Barco -> Int) -> Barco -> Barco -> Bool
tieneMas valor unBarco otroBarco = valor unBarco > valor otroBarco

robarseLosRecursos :: Barco -> Barco -> Barco
robarseLosRecursos barcoLadron barcoRobado = mapOro (oro barcoRobado) . mapMadera (madera barcoRobado) . robarseTodoTipoDeBalas barcoRobado $ barcoLadron

robarseTodoTipoDeBalas :: Barco -> Barco -> Barco
robarseTodoTipoDeBalas barcoRobado = mapBalasCanion (balasDeCanion barcoRobado) . mapBalas (balas barcoRobado)

mapMadera :: Int -> ModificarBarco
mapMadera valor barco = barco {madera = valor + madera barco}

mapBalas :: Int -> ModificarBarco
mapBalas valor barco = barco {balas = valor + balas barco}

mapBalasCanion :: Int -> ModificarBarco
mapBalasCanion valor barco = barco {balasDeCanion = valor + balasDeCanion barco}

vaciarRecursos :: ModificarBarco
vaciarRecursos barco = barco {oro = 0, madera = 0, balas = 0, balasDeCanion = 0}

type Suceso = Barco -> Barco

embarcarUnTesoro :: Int -> Suceso
embarcarUnTesoro peso = mapOro peso . dividirLaCarga peso

type ModificarTripulacion = Barco -> Barco

dividirLaCarga :: Int -> ModificarTripulacion
dividirLaCarga peso barco = barco {tripulacion = map (transportarCarga peso) (tripulacion barco)}

enfrentarUnBarco :: Barco -> Suceso
enfrentarUnBarco = flip enfrentamiento

encontrarCargamentoDeGrog :: Suceso
encontrarCargamentoDeGrog barco = barco {tripulacion = repetir 4 (map beberGrog) (tripulacion barco)}

repetir :: Int -> (a -> a) -> a -> a
repetir 0 _ elemento       = elemento
repetir n funcion elemento = repetir (n - 1) funcion (funcion elemento)

enfrentarEjercitoDeEsqueletos :: Int -> Suceso
enfrentarEjercitoDeEsqueletos cantidad barco = barco{tripulacion = (repetir cantidad enfrentarEsqueleto . primerTripulanteQue estaVivo . tripulacion $ barco) : drop 1 (tripulacion barco)}

primerTripulanteQue :: (Tripulante -> Bool) -> [Tripulante] -> Tripulante
primerTripulanteQue _ [] = error "ningun tripulante cumple, la cagaste en algo Juli"
primerTripulanteQue condicion (x : xs)
  |condicion x  = x
  |otherwise  = primerTripulanteQue condicion xs

estaVivo :: Tripulante -> Bool
estaVivo = not . estaMuerto

pasarPorTiendaDeGrog :: Suceso
pasarPorTiendaDeGrog barco
  |oro barco < 30                  = barco
  |todaLaTripulacionEstaViva barco = barco
  |otherwise                       = mapOro (-30) . revivirAlPrimerTripulante $ barco

todaLaTripulacionEstaViva :: Barco -> Bool
todaLaTripulacionEstaViva = all estaVivo . tripulacion

revivirAlPrimerTripulante :: ModificarTripulacion
revivirAlPrimerTripulante barco = barco {tripulacion = (beberGrog . head . tripulacion $ barco) : drop 1 (tripulacion barco)}

hacerSuceso :: (Barco -> Barco) -> Barco -> Barco
hacerSuceso suceso barco
  |esBarcoFantasma barco = barco
  |otherwise             = suceso barco

type Travesia = [Suceso]

--fuerteDeLosCondenados :: Travesia
--fuerteDeLosCondenados = cobrarPremio 50 . hacerTravesia [enfrentarEjercitoDeEsqueletos 100, pasarPorTiendaDeGrog, embarcarUnTesoro 30]
--
--flameHeart :: Travesia
--flameHeart barco = cobrarPremio (200 * (cantidadTripulantesVivos barco)) . hacerTravesia [enfrentarUnBarco galeonFlameHeart, enfrentarUnBarco bergatinFlameHeart, encontrarCargamentoDeGrog, embarcarUnTesoro 150] $ barco
--
--laGirita :: Travesia
--laGirita barco = cobrarPremio (2 * oro barco) . hacerTravesia [repetir 4 pasarPorTiendaDeGrog, enfrentarEjercitoDeEsqueletos 10] $ barco
--
--hacerTravesia :: [Barco -> Barco] -> Barco -> Barco
--hacerTravesia sucesos barco = foldl hacerSuceso barco sucesos
--
--cobrarPremio :: Int -> Barco -> Barco
--cobrarPremio oro barco
--  |not . esBarcoFantasma = mapOro oro barco
--  |otherwise             = barco