import Text.Show.Functions

data Ladron = Ladron{
    nombreLadron :: String,
    habilidades :: [String],
    armas :: [Arma]
}deriving Show

data Rehen = Rehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    plan :: [Rebelion]
}deriving Show

type Arma = Rehen -> Rehen

tokio :: Ladron
tokio = Ladron "tokio" ["trabajo psicologico","entrar en moto"] [pistola 9,pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "profesor" ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "pablo" 40 30 [esconderse]

arturito :: Rehen
arturito = Rehen "arturito" 70 50 [esconderse, atacar pablo]

esInteligente :: Ladron -> Bool
esInteligente profesor = True
esInteligente ladron = (length . habilidades $ ladron) > 2

conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma arma ladron = ladron {armas = (arma : (armas ladron))}

pistola :: Int -> Arma
pistola calibre rehen = modificarNivelMiedo ((*3) . length . nombreRehen $ rehen) . modificarNivelComplot (-5 * calibre) $ rehen

ametralladora :: Int -> Arma
ametralladora balas rehen = modificarNivelMiedo balas . modificarNivelComplot (-(div (nivelComplot rehen) 2)) $ rehen

modificarNivelMiedo :: Int -> Rehen -> Rehen
modificarNivelMiedo cantidad rehen = rehen{nivelMiedo = nivelMiedo rehen + cantidad}

modificarNivelComplot :: Int -> Rehen -> Rehen
modificarNivelComplot cantidad rehen = rehen{nivelComplot = nivelComplot rehen + cantidad}

type Intimidar = Ladron -> Rehen -> Rehen

disparar :: Intimidar
disparar ladron rehen = (armaQueIntimidaMas (armas ladron) rehen) $ rehen

armaQueIntimidaMas :: [Arma] -> Rehen -> Arma
armaQueIntimidaMas [armaSuperior] _ = armaSuperior
armaQueIntimidaMas (armaSuperior : siguiente : cola) rehen
  |(nivelMiedo . armaSuperior $ rehen) > (nivelMiedo . siguiente $ rehen) = armaQueIntimidaMas (armaSuperior : cola) rehen
  |otherwise = armaQueIntimidaMas (siguiente : cola) rehen

hacerseElMalo :: Intimidar
hacerseElMalo (Ladron "Berlin" habilidades armas) rehen = modificarNivelMiedo (cantidadLetrasHabilidades habilidades) $ rehen
hacerseElMalo (Ladron "Rio" habilidades armas) rehen = modificarNivelComplot 20 rehen
hacerseElMalo _ rehen = modificarNivelMiedo 10 rehen

cantidadLetrasHabilidades :: [String] -> Int
cantidadLetrasHabilidades = length . concat

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas ladron = map (dispararSi nivelComplot 60 ladron)

dispararSi :: (Rehen -> Int) -> Int -> Intimidar
dispararSi nivelAtributo valor ladron rehen
  |nivelAtributo rehen > valor = disparar ladron rehen
  |otherwise             = rehen

puedeEscaparse :: Ladron -> Bool
puedeEscaparse ladron = any (elem "disfrazarse de") [habilidades ladron]

pintaMal :: [Ladron] -> [Rehen] -> Bool
pintaMal ladrones rehenes = (promedioCualidad nivelComplot rehenes) > ((* cantidadArmas ladrones) . promedioCualidad nivelMiedo $ rehenes)

promedioCualidad :: (Rehen -> Int) -> [Rehen] -> Int
promedioCualidad nivelCualidad rehenes = div (sum (map nivelCualidad rehenes)) (length rehenes)

cantidadArmas :: [Ladron] -> Int
cantidadArmas = length . concatMap armas

type Rebelion = Ladron -> Ladron

atacar :: Rehen -> Rebelion
atacar compañero = repetir (dividido 10 . length . nombreRehen $ compañero) quitarArma

quitarArma :: Ladron -> Ladron
quitarArma ladron = ladron {armas = take ((length . armas $ ladron) - 1) (armas ladron)}

repetir :: Int -> (a -> a) -> a -> a
repetir 0 _ persona = persona
repetir n accion persona = repetir (n-1) accion (accion persona)

dividido :: Int -> Int -> Int
dividido unNumero otroNumero = div otroNumero unNumero

esconderse :: Rebelion
esconderse ladron = repetir (dividido 3 . length . habilidades $ ladron) quitarArma ladron

rebelarse :: [Rehen] -> Rebelion
rebelarse rehenes ladron = foldl (flip ($)) ladron (conjuntoRebeliones (map (modificarNivelComplot (-10)) rehenes))

conjuntoRebeliones :: [Rehen] -> [Rebelion]
conjuntoRebeliones = concatMap plan

ejecutarPlanValencia :: [Rehen] -> [Ladron] -> Int
ejecutarPlanValencia rehenes = (*1000000) . cantidadArmas . todosContraTodos rehenes . conseguirAmetralladoras

conseguirAmetralladoras :: [Ladron] -> [Ladron]
conseguirAmetralladoras = map (conseguirArma (ametralladora 45))

todosContraTodos :: [Rehen] -> [Ladron] -> [Ladron] --funcion todos los rehenes se rebelan con todos los ladrones
todosContraTodos rehenes = map (rebelarse rehenes)