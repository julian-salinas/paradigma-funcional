import Text.Show.Functions

--datas dados por la consigna
data Elemento = Elemento {
    tipo :: String,
    ataque :: Personaje-> Personaje,
    defensa :: Personaje-> Personaje
}deriving Show

data Personaje = Personaje {
    nombre :: String,
    salud :: Int,
    elementos :: [Elemento],
    anioPresente :: Int
}deriving Show

--IMPORTANTE: Para resolver este parcial, reemplacé todos los "Float" por "Int"

type Transformacion = Personaje -> Personaje

mandarAlAnio :: Int -> Transformacion
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Transformacion
meditar personaje = modificarSalud (div (salud personaje) 2) $ personaje

modificarSalud :: Int -> Transformacion
modificarSalud cantidad personaje = personaje {salud = max 0 (salud personaje + cantidad)}

causarDanio :: Int -> Transformacion
causarDanio cantidad = modificarSalud (-cantidad)

esMalvado :: Personaje -> Bool
esMalvado personaje = elem "Maldad" (map tipo . elementos $ personaje)

danioQueProduce :: Personaje -> Elemento -> Int
danioQueProduce personaje elemento = salud personaje - (salud . ataque elemento $ personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (puedeMatarlo personaje) enemigos

puedeMatarlo :: Personaje -> Personaje -> Bool
puedeMatarlo personaje enemigo = (any (tieneElementoMortal personaje) . elementos) enemigo

tieneElementoMortal :: Personaje -> Elemento -> Bool
tieneElementoMortal personaje elemento = salud personaje - danioQueProduce personaje elemento == 0

concentracion :: Int -> Elemento
concentracion nivel = Elemento {tipo = "Magia", defensa = (repetir nivel meditar)}

repetir :: Int -> (Personaje -> Personaje) -> Personaje -> Personaje
repetir   0   funcion heroe = heroe
repetir veces funcion heroe= repetir (veces - 1) funcion (funcion heroe)

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = take cantidad (repeat esbirro)

esbirro :: Elemento
esbirro = Elemento {tipo = "Maldad", ataque = (causarDanio 1)}

jack :: Personaje
jack = Personaje "Jack" 300 [concentracion 3, katanaMagica] 200

katanaMagica ::Elemento
katanaMagica = Elemento {tipo = "Magia", ataque = causarDanio 1000}

aku :: Int -> Int -> Personaje
aku anio salud = Personaje "Aku" salud (concentracion 4 : portalAlFuturo anio : esbirrosMalvados (100 * anio)) anio

portalAlFuturo :: Int -> Elemento
portalAlFuturo anio = Elemento "Magia" (mandarAlAnio (anio + 2800)) (aku (anio + 2800).salud)

luchar :: Personaje -> Personaje -> (Personaje,Personaje)
luchar atacante defensor
  |murio = (atacante,defensor)
  |otherwise = luchar defensor atacante
  where murio = matanAlDefensor (elementos atacante) defensor

matanAlDefensor :: [Elemento] -> Personaje -> Bool
matanAlDefensor elementos = (==0) . salud . aplicarTodosLosElementos elementos

aplicarTodosLosElementos :: [Elemento] -> Personaje -> Personaje
aplicarTodosLosElementos elementos personaje = foldl (flip ($)) personaje (map ataque elementos)

f :: (Int -> a -> (b, b)) -> (Int -> Int) -> Int -> [a] -> [b]
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))