import Text.Show.Functions

-- punto 1 --

data Heroe = Heroe{
  nombre         :: String,
  epiteto        :: String,
  reconocimiento :: Int,
  artefactos     :: [Artefacto],
  tareas         :: [Tarea]
}deriving Show

data Artefacto = Artefacto{
  nombreArtefacto :: String,
  rareza :: Int
}deriving Show


--punto 2 --


pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe
  |(>1000) . reconocimiento $ heroe = setEpiteto "El mitico" heroe
  |(>=500) . reconocimiento $ heroe = agregarArtefacto lanzaDelOlimpo . setEpiteto "El magnifico" $ heroe
  |(>100)  . reconocimiento $ heroe = agregarArtefacto xiphos . setEpiteto "Hoplita" $ heroe
  |otherwise                        = noHacerNada heroe

xiphos :: Artefacto
xiphos = Artefacto "xiphos" 50

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "lanza del olimpo" 100

setEpiteto :: String -> Heroe -> Heroe
setEpiteto epiteto heroe = heroe {epiteto = epiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe = heroe {artefactos = (artefacto :) . artefactos $ heroe}

noHacerNada :: Heroe -> Heroe
noHacerNada = id


-- punto 3 --


type Tarea = Heroe -> Heroe

encontrarArtefacto :: Artefacto -> Heroe -> Heroe
encontrarArtefacto artefacto = mapReconocimiento (+ rareza artefacto) . agregarArtefacto artefacto

mapReconocimiento :: (Int -> Int) -> Heroe -> Heroe
mapReconocimiento funcion heroe = heroe {reconocimiento = funcion . reconocimiento $ heroe}

escalarElOlimpo :: Tarea
escalarElOlimpo = agregarArtefacto relampagoDeZeus . mapArtefactos (filter ((>1000) . rareza)) . mapArtefactos (map (mapRareza (*3)))

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "el relampago de zeus" 500

mapArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefactos funcion heroe = heroe {artefactos = funcion . artefactos $ heroe}

mapRareza :: (Int -> Int) -> Artefacto -> Artefacto
mapRareza funcion artefacto = artefacto {rareza = funcion . rareza $ artefacto}

artefactosPrueba :: [Artefacto]
artefactosPrueba = [xiphos,lanzaDelOlimpo,relampagoDeZeus]

heroePrueba :: Heroe
heroePrueba = Heroe {nombre = "Julian",epiteto = "Hoplita", reconocimiento = 400, artefactos = artefactosPrueba, tareas = [escalarElOlimpo, ayudarACruzarLaCalle 10]}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras = mapEpiteto (++ (concat . replicate cuadras $ "o")) . setEpiteto "Groso"

mapEpiteto :: (String -> String) -> Heroe -> Heroe
mapEpiteto funcion heroe = heroe {epiteto = funcion . epiteto $ heroe}

matarALaBestia :: String -> (Heroe -> Bool) -> Tarea
matarALaBestia nombreBestia debilidad heroe
  |debilidad heroe = setEpiteto ("el asesino de " ++ nombreBestia) heroe
  |otherwise       = setEpiteto "el cobarde" . mapArtefactos (drop 1) $ heroe


-- punto 4 --


heracles2 :: Heroe
heracles2 = Heroe "Heracles 2" "Guardian del Olimpo" 700 [pistola,relampagoDeZeus] [matarAlLeonDeNemea]

pistola :: Artefacto
pistola = Artefacto "pistola" 1000


-- punto 5 --


matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarALaBestia "el leon de Nemea" ((>=20) . length . epiteto)


-- punto 5 --


hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea tarea = tarea


-- punto 6 --


presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir unHeroe otroHeroe
  |not . tienenIgualReconocimiento unHeroe $ otroHeroe = elQueTengaMasReconocimiento unHeroe otroHeroe
  |tienenIgualReconocimiento unHeroe otroHeroe = elQueTengaMasRareza unHeroe otroHeroe
  |otherwise = presumir (hacerTareasDelOtro unHeroe otroHeroe) (hacerTareasDelOtro otroHeroe unHeroe)

tienenIgualReconocimiento :: Heroe -> Heroe -> Bool
tienenIgualReconocimiento unHeroe otroHeroe = (== reconocimiento otroHeroe) . reconocimiento $ unHeroe

elQueTengaMasReconocimiento :: Heroe -> Heroe -> (Heroe, Heroe)
elQueTengaMasReconocimiento unHeroe otroHeroe = (mayorSegun reconocimiento unHeroe otroHeroe, menorSegun reconocimiento unHeroe otroHeroe)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

menorSegun :: Ord a => (p -> a) -> p -> p -> p
menorSegun f a b
  | f a < f b = a
  | otherwise = b

elQueTengaMasRareza :: Heroe -> Heroe -> (Heroe, Heroe)
elQueTengaMasRareza unHeroe otroHeroe = (mayorSegun totalRarezas unHeroe otroHeroe, menorSegun totalRarezas unHeroe otroHeroe)

totalRarezas :: Heroe -> Int
totalRarezas = sum . map rareza . artefactos

hacerTareasDelOtro :: Heroe -> Heroe -> Heroe
hacerTareasDelOtro unHeroe otroHeroe = foldl (flip hacerTarea) unHeroe (tareas otroHeroe)

{-
8) ¿Cuál es el resultado de hacer que presuman dos héroes con reconocimiento 100, ningún artefacto y
ninguna tarea realizada?
seria una infinita comparacion, no podriamos saber quien gana
-}

-- punto 9 --
type Labor = [Tarea]

realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor labor heroe = foldl (flip hacerTarea) heroe labor

{-
10) Si invocamos la función anterior con una labor infinita,
¿se podrá conocer el estado final del héroe? ¿Por qué?
No se puede, el heroe no pararía de realizar tareas, entonces el programa se tildaría
-}