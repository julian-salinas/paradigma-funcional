import Text.Show.Functions

doble = (*2)

-- funciones del enunciado


f1 (ns, nc, nf) = (ns + 1, nc + 2, nf + 3)
f2 (ns, nc, nf) = (ns, nc, nf + 5)
f3 (ns, nc, nf) = (ns, nc, nf - 3)

sinRepetidos [] = []
sinRepetidos (x:xs) = x : filter (/= x) (sinRepetidos xs)

misPociones = [felixFelices, multijugos]

invertir3 (a, b, c) = (c, b, a)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c


maxSegun f x y | f x > f y = x
               | otherwise = y

maximoSegun f xs = foldl1 (maxSegun f) xs

-- fin de funciones del enunciado


-- punto 1 -- 

data Persona = Persona{
    nombre  :: String,
    niveles :: (Suerte, Convencimiento, FuerzaFisica)
}deriving (Show,Eq)

type Suerte = Int

type Convencimiento = Int

type FuerzaFisica = Int

data Pocion = Pocion{
    nombrePocion :: String,
    receta       :: [Ingrediente]
}deriving Show

type Gramos = Int

harry :: Persona
harry = Persona "Harry Potter" (11, 5, 4)

ron :: Persona
ron = Persona "Ron Weasley" (6, 4, 6)

hermione :: Persona
hermione = Persona "Hermione Granger" (12, 8, 2)


-- punto 2


type Efecto = Persona -> Persona

data Ingrediente = Ingrediente{
    nombreIngrediente :: String,
    gramos :: Int,
    efectos :: [EfectoIngrediente]
}deriving Show

type TuplaNiveles = (Int,Int,Int)

type EfectoIngrediente = TuplaNiveles -> TuplaNiveles

felixFelices :: Pocion
felixFelices = Pocion "felixFelices" [escarabajosMachacados 52, ojoDeTigreSucio 2]

escarabajosMachacados :: Int -> Ingrediente
escarabajosMachacados gramos = Ingrediente "escarabajos machacados" gramos [f1,f2]

ojoDeTigreSucio :: Int -> Ingrediente
ojoDeTigreSucio gramos = Ingrediente "ojo de tigreSucio" gramos [f3]

cuernoBicornioEnPolvo :: Int -> Ingrediente
cuernoBicornioEnPolvo gramos = Ingrediente "cuerno de bicornio" gramos [invertir3, f1,f2]

sanguijuelasHormonales :: Int -> Ingrediente
sanguijuelasHormonales gramos = Ingrediente "sanguijuelas hormonales" gramos [mapTupla (*3), f3]

mapNiveles :: ((Int,Int,Int) -> (Int,Int,Int)) -> Efecto
mapNiveles f persona = persona {niveles = f .niveles $ persona}

mapTupla :: (Int -> Int) -> (Int, Int, Int) -> (Int,Int,Int)
mapTupla f (a,b,c) = (f a, f b, f c)

multijugos :: Pocion
multijugos = Pocion "multijugos" [cuernoBicornioEnPolvo 10, sanguijuelasHormonales 54]


-- punto 3 --


sumaDeNiveles :: TuplaNiveles -> Int
sumaDeNiveles (a,b,c) = a + b + c

diferenciaNiveles :: TuplaNiveles -> Int
diferenciaNiveles tupla = subtract (minimo tupla) . maximo $ tupla

minimo :: TuplaNiveles -> Int
minimo (a,b,c) = min a . min b $ c

maximo :: TuplaNiveles -> Int
maximo (a,b,c) = max a . max b $ c


-- punto 4 --


sumaNivelesPersona :: Persona -> Int
sumaNivelesPersona = sumaDeNiveles . niveles

diferenciaNivelesPersona :: Persona -> Int
diferenciaNivelesPersona = diferenciaNiveles . niveles


-- punto 5 --


efectosDePocion :: Pocion -> [EfectoIngrediente]
efectosDePocion = concat . map efectos . receta


-- punto 6 --


pocionesHeavies :: [Pocion] -> [String]
pocionesHeavies = map nombrePocion . filter ((>=4) . length . efectosDePocion)


-- punto 7 --


incluyeA :: Eq a => [a] -> [a] -> Bool
incluyeA unaLista otraLista = all (flip elem otraLista) unaLista


-- punto 8 --


esPocionMagica :: Pocion -> Bool
esPocionMagica pocion = (any (tieneTodasLasVocales . nombreIngrediente) . receta $ pocion) && (even . sumaDeGramos . receta $ pocion)

tieneTodasLasVocales :: String -> Bool
tieneTodasLasVocales = incluyeA vocales

vocales :: String
vocales = "aeiou"

sumaDeGramos :: [Ingrediente] -> Int
sumaDeGramos = sum . map gramos


-- punto 9 --


tomarPocion :: Pocion -> Efecto
tomarPocion pocion persona = foldl (flip hacerEfecto) persona (concatMap efectos . receta $ pocion)

hacerEfecto :: EfectoIngrediente -> Efecto
hacerEfecto = mapNiveles


-- punto 10 --


esAntidoto :: Persona -> Pocion -> Pocion -> Bool
esAntidoto persona unaPocion otraPocion = tomarseTodasLasPociones [unaPocion,otraPocion] persona == persona

tomarseTodasLasPociones :: [Pocion] -> Efecto
tomarseTodasLasPociones pociones persona = foldl (flip tomarPocion) persona pociones


--punto 11 --


personaMasAfectada :: Ord a => Pocion -> (Persona -> a) -> [Persona] -> Persona
personaMasAfectada pocion ponderacion personas = maximoSegun ponderacion (map (tomarPocion pocion) personas)


-- punto 12 --

{-
--Mostrar consultas que, usando la función del punto anterior, respondan la persona que quedó más afectada según las siguientes ponderaciones.

-suma de niveles (suerte, poder de convencimiento y fuerza física)
*Main> personaMasAfectada multijugos sumaNivelesPersona [harry, ron, hermione]
Persona {nombre = "Hermione Granger", niveles = (9,30,57)}


-promedio de niveles (puede ser el promedio entero)
*Main> personaMasAfectada multijugos promedioNivelesPersona [harry, ron, hermione]
Persona {nombre = "Hermione Granger", niveles = (9,30,57)}

-fuerza física
*Main> personaMasAfectada multijugos (trd3 . niveles) [harry, ron, hermione]
Persona {nombre = "Hermione Granger", niveles = (9,30,57)}

ACLARACION: utilizo tr3 . niveles para acceder a la fuerza física de una persona, ya que
sus atributos estan dados como una tupla


-diferencia de niveles
*Main> personaMasAfectada multijugos diferenciaNivelesPersona [harry, ron, hermione]
Persona {nombre = "Hermione Granger", niveles = (9,30,57)}

-}

promedioNivelesPersona :: Persona -> Int
promedioNivelesPersona = flip div 3 . sumaNivelesPersona


-- punto 13 --


agregarInfinitosIngredientes :: Int -> [Ingrediente] -> Pocion -> Pocion
agregarInfinitosIngredientes iteracion [] pocion       = agregarInfinitosIngredientes iteracion        (receta pocion)  pocion
agregarInfinitosIngredientes iteracion (x : xs) pocion = agregarInfinitosIngredientes (iteracion + 1)       xs          (aniadirIngrediente iteracion x $ pocion)

aniadirIngrediente :: Int -> Ingrediente -> Pocion -> Pocion
aniadirIngrediente gramos ingrediente pocion = pocion {receta = (++ [ingrediente]) . receta $ pocion}

pocionVacia :: Pocion
pocionVacia = Pocion {nombrePocion = "superPocion"}

superPocion :: [Ingrediente] -> Pocion
superPocion ingredientes = agregarInfinitosIngredientes 0 ingredientes pocionVacia

{-
aclaracion de lo que estoy haciendo en este punto
iteracion es el gramo que va a tener el ingrediente que se esta agregando, en cada iteracion se agrega
un ingrediente, cuando los ingredientes se acaban, la funcion toma los ingredientes que ya estan agregados
y los sigue iterando, ya que conserva el numero que indica los gramos que tiene que tener el ingrediente
que se esta agregando, generando una lista infinita de ingredientes en la superPocion
-}