import Text.Show.Functions
import Data.List

-- 1) Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema]
} deriving (Show)

data Personaje = Personaje{
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Show)

type Universo = [Personaje]

-- Personajes de prueba
drStrange :: Personaje
drStrange = Personaje 40 100 ["Magia"] "Doctor Strange" "Tierra"

wolverine :: Personaje
wolverine = Personaje 90 120 ["Garras", "Regeneracion"] "Wolverine" "Tierra"

spiderman :: Personaje
spiderman = Personaje 22 110 ["Trepar", "Tirar telarañas"] "Spiderman" "Tierra"

universoDePrueba :: Universo
universoDePrueba = [wolverine, drStrange]

chasquear :: Guantelete -> Universo -> Universo 
chasquear unGuantelete unosPersonajes
    | sePuedeChasquear unGuantelete = borrarMitadPoblacion unosPersonajes
    | otherwise = unosPersonajes

sePuedeChasquear :: Guantelete -> Bool
sePuedeChasquear unGuantelete = 
    ((=="uru") . material) unGuantelete && ((==6) . length . gemas) unGuantelete

borrarMitadPoblacion :: Universo -> Universo
borrarMitadPoblacion unUniverso = take (length unUniverso `div` 2) unUniverso 


-- 2)

-- a. Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = (>0) . length . filter ((<45).edad)

-- b. Saber la energía total de un universo, que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.
energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso = sum . map energia . filter tieneMasDeUnaHabilidad

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad = (>1) . length . habilidades

-- 3) Modelar lo que hacen las gemas

type Gema = Personaje -> Personaje

mente :: Int -> Gema
mente unValorDado = mapEnergia (subtract unValorDado)

alma :: String -> Gema
alma unaHabilidad = mapHabilidades (filter (/=unaHabilidad)) . mapEnergia (\x -> x-10)

espacio :: String -> Gema
espacio unPlaneta = mapPlaneta (const unPlaneta) . mapEnergia (\x -> x-20)

poder :: Gema
poder unPersonaje
    | tieneMenosDeDosHabilidades = quitarHabilidades . vaciarEnergia $ unPersonaje
    | otherwise = vaciarEnergia unPersonaje
    where tieneMenosDeDosHabilidades = (<2) . length $ habilidades unPersonaje


vaciarEnergia :: Personaje -> Personaje
vaciarEnergia = mapEnergia (const 0)

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades = mapHabilidades (drop 2)

tiempo :: Gema
tiempo unPersonaje 
    | noEsMenorDeEdad = mapEdad (`div` 2) unPersonaje
    | otherwise = mapEdad (const 18) unPersonaje
    where noEsMenorDeEdad = (>=18) . edad $ mapEdad (`div` 2) unPersonaje

loca :: Gema -> Gema
loca unaGema = unaGema . unaGema

-- Funciones auxiliares:
mapEnergia :: (Int -> Int) -> Personaje -> Personaje 
mapEnergia unaFuncion unPersonaje = 
    unPersonaje { energia = unaFuncion . energia $ unPersonaje }

mapPlaneta :: (String -> String) -> Personaje -> Personaje
mapPlaneta unaFuncion unPersonaje = 
    unPersonaje { planeta = unaFuncion . planeta $ unPersonaje }

mapEdad :: (Int -> Int) -> Personaje -> Personaje
mapEdad unaFuncion unPersonaje = 
    unPersonaje { edad = unaFuncion . edad $ unPersonaje }

mapHabilidades :: ([String] -> [String]) -> Personaje -> Personaje
mapHabilidades unaFuncion unPersonaje = 
    unPersonaje { habilidades = unaFuncion . habilidades $ unPersonaje }


{-
4) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma 
que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder
del alma tratando de eliminar la “programación en Haskell”.
-}

guanteleteEjemplo :: Guantelete
guanteleteEjemplo = Guantelete "Goma" [tiempo, alma "usar Mjolnir", loca $ alma "programacion en Haskell"]

{-
5) No se puede utilizar recursividad. 
Generar la función "utilizar" que dado una lista de gemas y un enemigo ejecuta
el poder de cada una de las gemas que lo componen contra el personaje dado. 
Indicar cómo se produce el “efecto de lado” sobre la víctima.
-}

utilizar :: [Gema] -> Personaje -> Personaje
utilizar unasGemas unEnemigo = foldl (flip ($)) unEnemigo unasGemas 
{- 
Este ejercicio consta en aplicar sucesivamente una lista de funciones sobre un mismo elemento
Ejemplo: aplico primer gema sobre un personaje, luego al personaje que resulta 
le vuelvo a aplicar la gema que le sigue, luego a este ultimo la que sigue y asi sucesivamente
-}


{-
6) Resolver utilizando recursividad. 
Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene 
la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 
-}

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje = obtenerGemaMasPoderosa (gemas unGuantelete) unPersonaje

obtenerGemaMasPoderosa :: [Gema] -> Personaje -> Gema
obtenerGemaMasPoderosa [x] _ = x
obtenerGemaMasPoderosa (x:x':xs) unPersonaje
    | primeraGemaGeneraPerdidaMasGrande x x' unPersonaje = obtenerGemaMasPoderosa (x:xs) unPersonaje
    | otherwise = obtenerGemaMasPoderosa (x':xs) unPersonaje 

primeraGemaGeneraPerdidaMasGrande :: Gema -> Gema -> Personaje -> Bool
primeraGemaGeneraPerdidaMasGrande unaGema otraGema unPersonaje = 
    diferenciaDeEnergias unaGema unPersonaje > diferenciaDeEnergias otraGema unPersonaje

diferenciaDeEnergias :: Gema -> Personaje -> Int
diferenciaDeEnergias gema personaje = energia (gema personaje) - energia personaje


-- 7) Dada la función generadora de gemas y un guantelete de locos:

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema : (infinitasGemas gema) 
-- Genera una lista infinita de una misma gema

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo) 
-- Declara una funcion de tipo "Guantelete" que tiene como gemas una lista infinita de la gema del tiempo

-- Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete
-- Esta funcion toma las primeras 3 gemas del guantelete y las "utiliza" (o sea las aplica) a un personaje


{-
Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
    - gemaMasPoderosa punisher guanteleteDeLocos
    - usoLasTresPrimerasGemas guanteleteDeLocos punisher
-}

{-
Respuestas:
    - La primer funcion se puede ejecutar ya que, gracias a la evaluacion diferida, 
      automaticamente reconoce a la unica gema que se encuentra en la lista como la 
      mas poderosa.
    - En este caso utiliza unicamente las primeras 3 funciones que hay en la lista infinita
      nuevamente gracias a la evaluacion diferida, sin necesidad de esperar evaluar todas las
      otras funciones que hay en ella.
-}