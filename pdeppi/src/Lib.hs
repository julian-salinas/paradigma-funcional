import Text.Show.Functions

data Persona = Persona{
    nombrePersona :: String,
    direccion :: String,
    dinero :: Int,
    comidaFavorita :: Comida,
    cupones :: [Cupon]
}deriving Show

data Comida = Comida{
    nombre :: String,
    costo :: Int,
    ingredientes :: [Ingrediente]
}deriving Show

type Cupon = Comida -> Comida
type Ingrediente = String

paula :: Persona
paula = Persona "Paula" "Thames 1585" 3600 hamburguesaDeluxe []

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida "hamburguesa deluxe" 350 ["pan", "carne", "lechuga", "tomate", "panceta","queso","huevo frito"]

julian :: Persona
julian = Persona "Julian" "Palacios 210" 10000 nioquis [esoNoEsCocaPapi "la bichy ahora", largaDistancia]

nioquis :: Comida
nioquis = Comida "nioquis" 400 ["papa","huevo","harna","queso"]

comprar :: Comida -> Persona -> Persona
comprar comida persona
  |(&& ((<200) . costo $ comida)) . (>= costo comida) . dinero $ persona = hacerComidaFavorita comida . concretarCompra comida $ persona
  |(>= costo comida) . dinero $ persona = concretarCompra comida $ persona
  |otherwise = persona

concretarCompra :: Comida -> Persona -> Persona
concretarCompra comida = mapDinero (-(costo comida))

mapDinero :: Int -> Persona -> Persona
mapDinero valor persona = persona {dinero = (+ valor) . dinero $ persona}

hacerComidaFavorita :: Comida -> Persona -> Persona
hacerComidaFavorita comida persona = persona {comidaFavorita = comida}

carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras comidas = mapDinero (-100) . comprarTodo comidas

comprarTodo :: [Comida] -> Persona -> Persona
comprarTodo comidas persona = foldl (flip comprar) persona comidas

semanaVegana :: Cupon
semanaVegana comida
  |all (ingredienteEs vegano) . ingredientes $ comida = descontarPorcentajeCosto 50 comida
  |otherwise                                          = comida

esoNoEsCocaPapi :: String -> Cupon
esoNoEsCocaPapi bebida = agregarAlNombre "Party" . agregarIngrediente bebida

sinTACCis :: Cupon
sinTACCis = agregarEnCadaIngrediente " libre de gluten"

findeVegetariano :: Cupon
findeVegetariano comida
  |all (ingredienteEs vegetariano) (ingredientes comida) = descontarPorcentajeCosto 30 comida
  |otherwise                                             = comida

largaDistancia :: Cupon
largaDistancia = mapCosto (-50) . perderIngredientesMasPesadosQue 10

ingredienteEs :: [String] -> Ingrediente -> Bool
ingredienteEs ingredientesNoPermitidos ingrediente = not . elem ingrediente $ ingredientesNoPermitidos

vegano :: [String]
vegano = ["carne","huevo","queso"]

vegetariano :: [String]
vegetariano = ["carne"]

--esVegano :: Ingrediente -> Bool
--esVegano ingrediente = not . elem ingrediente $ alimentosNoVeganos
--
--esVegetariano :: Ingrediente -> Bool
--esVegetariano ingrediente = not . elem ingrediente $ alimentosNoVeganos

descontarPorcentajeCosto :: Int -> Comida -> Comida
descontarPorcentajeCosto cantidad comida = comida{costo = subtract (porcentaje cantidad (costo comida)) (costo comida)}

porcentaje :: Int -> Int -> Int
porcentaje unNumero = flip div 100 . (*unNumero)

agregarAlNombre :: String -> Comida -> Comida
agregarAlNombre unString comida = comida {nombre = (++ unString) . nombre $ comida}

agregarIngrediente :: Ingrediente -> Comida -> Comida
agregarIngrediente ingrediente comida = comida{ ingredientes = (ingrediente :) . ingredientes $ comida}

agregarEnCadaIngrediente :: String -> Comida -> Comida
agregarEnCadaIngrediente unString comida = comida {ingredientes = map (++ unString) . ingredientes $ comida}

mapCosto :: Int -> Comida -> Comida
mapCosto valor comida = comida {costo = (+ valor) . costo $ comida}

perderIngredientesMasPesadosQue :: Int -> Comida -> Comida
perderIngredientesMasPesadosQue peso comida = comida {ingredientes = filter ((<peso) . pesoIngrediente) . ingredientes $ comida}

pesoIngrediente :: Ingrediente -> Int
pesoIngrediente = length

comprarConCupones :: Persona -> Persona
comprarConCupones persona = comprar (aplicarTodosLosCupones (comidaFavorita persona) (cupones persona)) persona

aplicarTodosLosCupones :: Comida -> [Cupon] -> Comida
aplicarTodosLosCupones = foldl (flip ($))

superComida :: [Comida] -> Comida
superComida comidas = Comida {nombre = todosLosNombresSinVocales comidas, costo = sumarTodosLosCostos comidas, ingredientes = concatenarTodosLosIngredientes comidas}

todosLosNombresSinVocales :: [Comida] -> String
todosLosNombresSinVocales = sacarVocales .concatMap nombre

sacarVocales :: String -> String
sacarVocales = filter (not . esVocal)

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

sumarTodosLosCostos :: [Comida] -> Int
sumarTodosLosCostos = sum . map costo

concatenarTodosLosIngredientes :: [Comida] -> [Ingrediente]
concatenarTodosLosIngredientes = concatMap ingredientes

compraDeluxe :: [Comida] -> Persona -> Persona
compraDeluxe comidas = comprar (comidaCompraDeluxe comidas)

comidaCompraDeluxe :: [Comida] -> Comida
comidaCompraDeluxe = superComida . map duplicarCosto . filter ((<400) . costo)

duplicarCosto :: Comida -> Comida
duplicarCosto comida = comida{costo = (*2) . costo $ comida} 
