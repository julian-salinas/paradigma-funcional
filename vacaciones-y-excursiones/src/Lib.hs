data Turista = Turista{
    nivelCansancio :: Int,
    nivelStress :: Int,
    viajoSolo :: Bool,
    idiomas :: [String]
}deriving Show

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista
  |viajoSolo turista = modificarCansancio (-5) $ turista
  |otherwise = modificarStress (-1) $ turista

modificarCansancio :: Int -> Turista -> Turista
modificarCansancio cantidad turista = turista {nivelCansancio = nivelCansancio turista + cantidad}

modificarStress :: Int -> Turista -> Turista
modificarStress cantidad turista = turista {nivelStress = nivelStress turista + cantidad}

apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemento turista = modificarStress ((* (-1)) . length $ elemento) $ turista

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma idioma turista = estarAcompañado . aprenderIdioma idioma $ turista

aprenderIdioma :: String -> Excursion
aprenderIdioma idioma turista = turista{idiomas = idioma : (idiomas turista)}

estarAcompañado :: Turista -> Turista
estarAcompañado turista = turista {viajoSolo = False}

caminar :: Int -> Excursion
caminar minutos turista = modificarCansancio (intensidadDeLaCaminad minutos) . modificarStress (-1 * (intensidadDeLaCaminad minutos)) $ turista

intensidadDeLaCaminad :: Int -> Int
intensidadDeLaCaminad minutos = div 4 minutos

data Marea = Fuerte | Moderada | Tranquila deriving(Show,Eq)

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte turista = modificarStress 6 . modificarCansancio 10 $ turista
paseoEnBarco Moderada turista = turista
paseoEnBarco Tranquila turista = caminar 10 . apreciarPaisaje "mar" . salirAHablarUnIdioma "aleman" $ turista

ana :: Turista
ana = Turista 0 21 False ["espaniol"]

beto :: Turista
beto = Turista 15 15 True ["aleman"]

cathi :: Turista
cathi = Turista 15 15 True ["aleman", "catalan"]

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = restarDiezPorCientoStress . excursion $ turista

restarDiezPorCientoStress :: Turista -> Turista
restarDiezPorCientoStress turista = turista {nivelStress = nivelStress turista - (diezPorCiento . nivelStress $ turista)}

diezPorCiento :: Int -> Int
diezPorCiento numero = div numero 10

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun elemento turista excursion = (elemento .(hacerExcursion excursion) $ turista) - elemento turista 

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = (length . idiomas . (hacerExcursion excursion) $ turista) > (length . idiomas $ turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = (>=3) . diferenciaStress (hacerExcursion excursion) $ turista

diferenciaStress :: Excursion -> Turista -> Int
diferenciaStress excursion turista = nivelStress turista - (nivelStress . hacerExcursion excursion $ turista)

type Tour = [Excursion]

completo :: Tour
completo = [salirAHablarUnIdioma "melmacquiano", irALaPlaya, caminar 40, apreciarPaisaje "cascada", caminar 20]

ladoB :: Excursion -> Tour
ladoB excursion = [caminar 180, excursion, paseoEnBarco Tranquila]

islaVecina :: Marea -> Tour
islaVecina Fuerte = [paseoEnBarco Fuerte, apreciarPaisaje "lago", paseoEnBarco Fuerte]
islaVecina Moderada = [paseoEnBarco Moderada, irALaPlaya, paseoEnBarco Moderada]
islaVecina Tranquila = [paseoEnBarco Tranquila, irALaPlaya, paseoEnBarco Tranquila]

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = hacerTodasLasExcursiones tour . modificarStress (length tour) $ turista 

hacerTodasLasExcursiones :: Tour -> Turista -> Turista
hacerTodasLasExcursiones tour turista = foldr (hacerExcursion) turista tour

algunoConvincente :: [Tour] -> Turista -> Bool
algunoConvincente tours turista = any (esConvincente turista) tours

esConvincente :: Turista -> Tour -> Bool
esConvincente turista tour = ((&&) (estaAcompaniado turista)) . (algunaExcursionDesestresante tour) $ turista

algunaExcursionDesestresante :: Tour -> Turista -> Bool
algunaExcursionDesestresante tour turista = any (esDesestresante turista) tour

estaAcompaniado :: Turista -> Bool
estaAcompaniado turista = not . viajoSolo $ turista 

efectividadTour :: Tour -> [Turista] -> Int
efectividadTour tour turistas = (calcularEspiritualidad turistas) - (calcularEspiritualidad . todosHacerTour tour $ turistas)

todosHacerTour :: Tour -> [Turista] -> [Turista]
todosHacerTour tour turistas = map (hacerTodasLasExcursiones tour) turistas

calcularEspiritualidad :: [Turista] -> Int
calcularEspiritualidad turistas = calcularTotal nivelStress turistas + calcularTotal nivelCansancio turistas

calcularTotal :: (Turista -> Int) -> [Turista] -> Int
calcularTotal elemento = sum . map (elemento)