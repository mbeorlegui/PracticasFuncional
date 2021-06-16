import Text.Show.Functions
import Data.List

-- Parte A
data Persona = Persona {
    nombre :: String,
    direccion :: String,
    dinero :: Int,
    comidaFav :: Comida,
    cupones :: [Cupon]
} deriving (Show)

data Comida = Comida {
    nombreComida :: String,
    costo :: Int,
    ingredientes :: [String]
} deriving (Show, Eq)

type Bebida = String

paula :: Persona
paula = Persona {
    nombre = "Paula" ,
    direccion = "Thames 1585",
    dinero = 3600,
    comidaFav = hamburguesaDeluxe,
    cupones = []
}

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida{
    nombreComida = "Hamburguesa deluxe", 
    costo = 350, 
    ingredientes = ["pan", "carne", "lechuga", "tomate","panceta","queso","huevo frito"]
}

yo :: Persona
yo = Persona{
  nombre = "Matias",
  direccion = "Calle falsa 123",
  dinero = 10000000,
  comidaFav = asado,
  cupones = [semanaVegana, sinTACCis]
}

asado :: Comida
asado = Comida{
    nombreComida = "Asadazo",
    costo = 15,
    ingredientes = ["carne", "sal"]
}

-- Parte B
{-
1) comprar: cuando una persona compra una comida se descuenta el costo de la
misma de su dinero disponible (¡ojo! No se puede comprar si no alcanza la plata).
Además, si salió menos de $200 se vuelve su nueva comida favorita.
-}

comprar :: Comida -> Persona -> Persona
comprar comida persona
  | leAlcanzaElDinero && comidaValeMasDe200     = efectuarCompra comida persona
  | leAlcanzaElDinero && not comidaValeMasDe200 = (cambiarComidaFav comida) . (efectuarCompra comida) $ persona
  | otherwise                                   = persona
  where leAlcanzaElDinero  = (dinero persona) - (costo comida) >= 0
        comidaValeMasDe200 = costo comida > 200


efectuarCompra :: Comida -> Persona -> Persona
efectuarCompra comida = (cambiarComidaFav comida) . (descontarCostoComida comida)

descontarCostoComida :: Comida -> Persona -> Persona
descontarCostoComida comida = mapDinero (subtract (costo comida))

cambiarComidaFav :: Comida -> Persona -> Persona
cambiarComidaFav nuevaComidaFav = mapComidaFav (const nuevaComidaFav)

mapComidaFav :: (Comida -> Comida) -> Persona -> Persona 
mapComidaFav unaFuncion unaPersona = 
  unaPersona { comidaFav = unaFuncion . comidaFav $ unaPersona }

mapDinero :: (Int -> Int) -> Persona -> Persona 
mapDinero unaFuncion unaPersona = 
  unaPersona { dinero = unaFuncion . dinero $ unaPersona }

mapCosto :: (Int -> Int) -> Comida -> Comida
mapCosto unaFuncion unaComida = 
  unaComida { costo = unaFuncion . costo $ unaComida }

{-
2) carritoDeCompras: en nuestra aplicación se pueden comprar muchas comidas al
mismo tiempo. Lamentablemente usar este servicio hace que el empaque sea más
pesado, por lo que se suma $100 más al total.
-}

carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras comidas unaPersona = 
  foldl (\persona unaComida -> comprar unaComida persona) (cobrarExtra 100 unaPersona) comidas
-- \persona comida1 -> comprar comida1 persona

cobrarExtra :: Int -> Persona -> Persona
cobrarExtra unExtra = mapDinero (+unExtra)

type Cupon = Comida -> Comida

semanaVegana :: Cupon
semanaVegana comida
  | comidaEsVegana comida = mapCosto (`div` 2) comida
  | otherwise             = comida

comidaEsVegana :: Comida -> Bool
comidaEsVegana comida = 
  not (contieneIngrediente "carne" comida || contieneIngrediente "huevo" comida || contieneIngrediente "queso" comida)

contieneIngrediente :: String -> Comida -> Bool
contieneIngrediente unIngrediente = any (==unIngrediente) . ingredientes

--

esoNoEsCocaPapi :: Bebida -> Cupon
esoNoEsCocaPapi unaBebida = 
  mapIngredientes (++ [unaBebida]) . mapNombre (++ " Party")

mapIngredientes :: ([String] -> [String]) -> Comida -> Comida
mapIngredientes unaFuncion unaComida = 
  unaComida { ingredientes = unaFuncion . ingredientes $ unaComida }

mapNombre :: (String -> String) -> Comida -> Comida
mapNombre unaFuncion unaComida = 
  unaComida { nombreComida = unaFuncion . nombreComida $ unaComida }

--

sinTACCis :: Cupon
sinTACCis = mapIngredientes (map (++" libre de gluten"))

--

findeVegetariano :: Cupon
findeVegetariano comida
  | esVegetariana = mapCosto ((`div` 100 ) . (*70)) comida
  | otherwise     = comida
  where esVegetariana = contieneIngrediente "carne" comida

--

{-
largaDistancia: este cupón es muy útil para las personas que viven lejos. Por solo
$50 pesos mas, Pdeppi puede llevar la comida hasta tu casa. Lamentablemente por
la lejanía, todos los ingredientes que tienen más de 10 letras se pierden en el
camino .
-}

largaDistancia :: Cupon
largaDistancia = perderIngredientesConMasDe10Letras . mapCosto (+50)

perderIngredientesConMasDe10Letras :: Comida -> Comida
perderIngredientesConMasDe10Letras = mapIngredientes (filter (not.tieneMasDe10Letras))

tieneMasDe10Letras :: String -> Bool
tieneMasDe10Letras = (>10).length


-- Parte B
{-
1. comprarConCupones: nos permite que una persona realice la compra de su
comida favorita aplicándole todos los cupones que tiene a su disposición.
-}
comprarConCupones :: Persona -> Comida -> Persona
comprarConCupones unaPersona unaComida = comprar (aplicarCupones (cupones unaPersona) unaComida) unaPersona

aplicarCupones :: [Cupon] -> Comida -> Comida
aplicarCupones []      comida = comida
aplicarCupones [x]     comida = x comida
aplicarCupones (x:xs)  comida = aplicarCupones xs (x comida) 


{-
2. superComida: dado un conjunto de comidas. Se genera una gran comida, que su
precio es la sumatoria de todos los precios, su nombre es el conjunto de todos los
nombres sacando las vocales y sus ingredientes son todos los ingredientes juntos
sin repetidos.
-}
superComida :: [Comida] -> Comida
superComida unasComidas = 
  Comida{
    nombreComida = eliminarVocales . juntarNombres $ unasComidas,
    costo = sumatoriaDePrecios unasComidas,
    ingredientes = eliminarRepetidos . juntarIngredientes $ unasComidas
  }

-- su precio es la sumatoria de todos los precios

sumatoriaDePrecios :: [Comida] -> Int
sumatoriaDePrecios = sum . (map costo)

-- su nombre es el conjunto de todos los nombres sacando las vocales

juntarNombres :: [Comida] -> String
juntarNombres unasComidas = concatMap ((" "++) . nombreComida) unasComidas

eliminarVocales :: String -> String
eliminarVocales = filter (not.esVocal)

esVocal :: Char -> Bool
esVocal letra = elem letra ['a','e','i','u','u']


-- sus ingredientes son todos los ingredientes juntos sin repetidos

juntarIngredientes :: [Comida] -> [String]
juntarIngredientes = concatMap ingredientes

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (filter (/= x) xs)


{-
3. compraDeluxe: hace que una persona compre una súper comida creada a partir de
un conjunto de comidas. Para crearla solo se utilizarán aquellas que cuesten menos
de $400, pero duplicándoles el precio.
-}

compraDeluxe :: [Comida] -> Persona -> Persona
compraDeluxe comidas = comprar $ superComida (duplicarPrecioComidas . seleccionarComidas $ comidas) 

seleccionarComidas :: [Comida] -> [Comida]
seleccionarComidas = filter ((<400) . costo)

duplicarPrecioComidas :: [Comida] -> [Comida]
duplicarPrecioComidas = map (mapCosto (*2))
