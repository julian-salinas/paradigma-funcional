import Text.Show.Functions

data Guantelete = Guantelete {
    material :: String,
    gemas :: [GemaDelInfinito]
}deriving (Show)

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show,Eq)

data Universo = Universo{
    habitantes :: [Personaje]
}deriving Show

type CambiarUniverso = Universo -> Universo

drStrange = Personaje 44 20 ["magia"] "Dr Strange" "Tierra" 
ironMan = Personaje 50 20 ["volar","disparar","unibeam"] "Iron Man" "Tierra"
quill = Personaje 50 20 ["patada","disparar"] "quill" "Tierra"
groot = Personaje 50 20 ["I'm Groot"] "groot" "Planeta X"

universoPrueba :: Universo
universoPrueba = Universo [drStrange,ironMan,quill,groot]

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto (Guantelete material gemas) = (material == "uru") && ((==6) . length $ gemas)

chasquido :: CambiarUniverso
chasquido universo = Universo {habitantes = repetir (div (length . habitantes $ universo) 2) eliminarUnHabitante (habitantes universo)}

eliminarUnHabitante :: [Personaje] -> [Personaje]
eliminarUnHabitante habitantes = take (length habitantes - 1) habitantes

repetir :: Int -> ([Personaje] -> [Personaje]) -> [Personaje] -> [Personaje]
repetir cantidadVeces funcion lista
  |cantidadVeces > 0 = repetir (cantidadVeces - 1) funcion (funcion lista)
  |otherwise = lista

esAptoPendex :: Universo -> Bool
esAptoPendex universo = any (<45) (map edad (habitantes universo))

energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso universo = foldl (+) 0 (map energia (filter ((>1) . length . habilidades) (habitantes universo)))

type GemaDelInfinito = Personaje -> Personaje

gemaDeLaMente :: Int -> GemaDelInfinito
gemaDeLaMente valor personaje = modificarEnergia (-valor) personaje

gemaDelAlma :: String -> GemaDelInfinito
gemaDelAlma habilidad personaje = modificarEnergia (-10) . quitarHabilidad habilidad $ personaje

gemaDelPoder :: GemaDelInfinito
gemaDelPoder personaje
  |masDeDosHabilidades = vaciarHabilidades . quitarEnergia $ personaje
  |otherwise = quitarEnergia personaje
  where masDeDosHabilidades = (>2) . length . habilidades $ personaje  

gemaDelTiempo :: GemaDelInfinito
gemaDelTiempo personaje = personaje {edad = menor 18 (div (edad personaje) 2), energia = (energia personaje) - 50}

gemaLoca :: GemaDelInfinito -> GemaDelInfinito
gemaLoca gema = gema.gema

menor :: Int -> Int -> Int
menor unNumero otroNumero
  |unNumero > otroNumero = unNumero
  |otherwise = otroNumero

modificarEnergia :: Int -> Personaje -> Personaje
modificarEnergia valor personaje = personaje {energia = (energia personaje) + valor}

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidad personaje = personaje {habilidades = filter (/=habilidad) (habilidades personaje)}

vaciarHabilidades :: Personaje -> Personaje
vaciarHabilidades personaje = personaje {habilidades = []}

quitarEnergia :: Personaje -> Personaje
quitarEnergia personaje = personaje{energia = 0}

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete "Goma" [gemaDelTiempo, gemaDelAlma "usar Mjolnir",gemaLoca (gemaDelAlma "programación en Haskell")]

utilizar :: [GemaDelInfinito] -> Personaje -> Personaje
utilizar gemas personaje = foldl (flip ($)) personaje gemas

gemaMasPoderosa :: Guantelete -> Personaje -> GemaDelInfinito
gemaMasPoderosa (Guantelete _ [gemaSuperior]) personaje = gemaSuperior
gemaMasPoderosa (Guantelete material (gemaSuperior:gemaAProbar:xs)) personaje
  |(energia . gemaAProbar $ personaje) < (energia . gemaSuperior $ personaje) = gemaMasPoderosa (Guantelete material (gemaAProbar:xs)) personaje
  |(energia . gemaAProbar $ personaje) > (energia . gemaSuperior $ personaje) = gemaMasPoderosa (Guantelete material (gemaSuperior:xs)) personaje