import Text.Show.Functions
import Data.List

-- 1) Modelar ladrones y rehenes

data Ladron = Ladron{
    nombreLadron    :: String,
    habilidades     :: [String],
    armas           :: [Arma]
} deriving (Show)

data Rehen = Rehen{
    nombreRehen     :: String,
    nivelComplot    :: Int,
    nivelMiedo      :: Int,
    plan          :: Plan
} deriving (Show)

type Arma = Rehen -> Rehen


-- 1)

pistola :: Int -> Arma
pistola unCalibre unRehen = mapComplot (`div` (5*unCalibre)) . mapMiedo ((*) $ largoNombrePorTres unRehen) $ unRehen 

ametralladora :: Int -> Arma
ametralladora cantidadBalas = mapComplot (`div` 2) . mapMiedo (+cantidadBalas)


-- Funciones auxiliares:
mapComplot :: (Int -> Int) -> Rehen -> Rehen 
mapComplot unaFuncion unRehen = 
    unRehen { nivelComplot = unaFuncion . nivelComplot $ unRehen }

mapMiedo :: (Int -> Int) -> Rehen -> Rehen 
mapMiedo unaFuncion unRehen = 
    unRehen { nivelMiedo = unaFuncion . nivelMiedo $ unRehen }

mapArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron 
mapArmas unaFuncion unLadron = 
    unLadron { armas = unaFuncion . armas $ unLadron }

largoNombrePorTres :: Rehen -> Int
largoNombrePorTres = (3*) . length . nombreRehen


rio :: Ladron
rio = Ladron "Rio" ["Hacerse el lindo", "hablar gallego"] [ametralladora 5, pistola 30]

berlin :: Ladron
berlin = Ladron "Berlin" ["Disparar"] [ametralladora 5, pistola 35]

-- 2)

-- b) Hacerse el malo
{-
    - Cuando el que se hace el malo es Berlín, aumenta el miedo del rehén tanto como la cantidad de letras que sumen sus habilidades.
    - Cuando Río intenta hacerse el malo, le sale mal y en cambio aumenta el nivel de complot del rehén en 20. 
    - En otros casos, el miedo del rehén sube en 10. 
-}

type Intimidacion = Ladron -> Rehen -> Rehen

dispararAlTecho :: Intimidacion
dispararAlTecho unLadron unRehen = (elegirArmaIndicada (armas unLadron) unRehen) unRehen


elegirArmaIndicada :: [Arma] -> Rehen -> Arma
elegirArmaIndicada [x] _ = x
elegirArmaIndicada (x:x':xs) unRehen
    | primerArmaGeneraMasMiedo x x' unRehen = elegirArmaIndicada (x :xs) unRehen
    | otherwise                             = elegirArmaIndicada (x':xs) unRehen

primerArmaGeneraMasMiedo :: Arma -> Arma -> Rehen -> Bool
primerArmaGeneraMasMiedo unArma otroArma unRehen =
    (nivelMiedo unRehen - nivelMiedo (unArma unRehen  )) >
    (nivelMiedo unRehen - nivelMiedo (otroArma unRehen))

hacerseElMalo :: Intimidacion
hacerseElMalo unLadron unRehen 
    | esBerlin     = cambiarMiedo unLadron unRehen
    | esRio        = mapComplot (+20) unRehen
    | otherwise    = mapComplot (+10) unRehen
    where esRio    = nombreLadron unLadron == "Rio"
          esBerlin = nombreLadron unLadron == "Berlin"

cambiarMiedo :: Intimidacion
cambiarMiedo unLadron = mapMiedo ((+) $ cantidadDeLetrasDeHabilidades unLadron)

cantidadDeLetrasDeHabilidades :: Ladron -> Int
cantidadDeLetrasDeHabilidades = sum . map length . habilidades 

tieneMasComplotQueMiedo :: Rehen -> Bool
tieneMasComplotQueMiedo unRehen = 
    nivelComplot unRehen > nivelMiedo unRehen

type Plan = Ladron -> Ladron

atacarAlLadron :: Rehen -> Plan
atacarAlLadron rehenAliado = 
    quitarArmas (cantidadLetrasNombre rehenAliado `div` 10)

quitarArmas :: Int -> Ladron -> Ladron
quitarArmas cantidad = mapArmas (drop cantidad)

esconderse :: Plan
esconderse unLadron = mapArmas (take (cantidadDeHabilidadesDividido3 unLadron)) unLadron

cantidadLetrasNombre :: Rehen -> Int
cantidadLetrasNombre = length . nombreRehen 

cantidadDeHabilidadesDividido3 :: Ladron -> Int
cantidadDeHabilidadesDividido3 = div 3 . length . habilidades

-- 1)

tokio :: Ladron
tokio = Ladron "Tokio" ["Trabajo priscologico", "Entrar en moto"] [pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 esconderse

arturito :: Rehen
arturito = Rehen "Arturito" 70 50 (atacarAlLadron pablo . esconderse)

ladrones :: [Ladron]
ladrones = [tokio, profesor]

rehenes :: [Rehen]
rehenes  = [pablo, arturito]


-- 2) 

ladronEsInteligente :: Ladron -> Bool
ladronEsInteligente unLadron = 
    esElProfesor unLadron || tieneMasDeDosHabilidades unLadron

esElProfesor :: Ladron -> Bool
esElProfesor = (=="Profesor") . nombreLadron

tieneMasDeDosHabilidades :: Ladron -> Bool
tieneMasDeDosHabilidades = (>2) . length . habilidades

-- 3) 

conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma unArma = 
    mapArmas ((:) unArma)


-- 4)

intimidarARehen :: Intimidacion -> Ladron -> Rehen -> Rehen
intimidarARehen unaIntimidacion unLadron = unaIntimidacion unLadron 


--5)

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas unLadron grupoRehenes =
    map (dispararAlTecho unLadron) grupoRehenes


-- 6)
puedeEscaparseDeLaPolicia :: Ladron -> Bool
puedeEscaparseDeLaPolicia unLadron =
    any empiezaConEscaparseDe (habilidades unLadron)

empiezaConEscaparseDe :: String -> Bool
empiezaConEscaparseDe = (== "disfrazarse de") . take 14

-- 7)

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal unosLadrones unosRehenes =
    promedioDeComplot unosRehenes > promedioDeMiedo unosRehenes * cantidadDeArmasTotales unosLadrones

promedioDeComplot :: [Rehen] -> Int
promedioDeComplot unosRehenes = (sumatoriaDeComplot unosRehenes) `div` (cantidadDeRehenes unosRehenes)

cantidadDeRehenes :: [Rehen] -> Int
cantidadDeRehenes = length . map nivelComplot

sumatoriaDeComplot :: [Rehen] -> Int
sumatoriaDeComplot = sum . map nivelComplot

promedioDeMiedo :: [Rehen] -> Int
promedioDeMiedo unosRehenes = (sumatoriaDeMiedo unosRehenes) `div` (cantidadDeRehenes unosRehenes)

sumatoriaDeMiedo :: [Rehen] -> Int
sumatoriaDeMiedo = sum . map nivelMiedo

cantidadDeArmasTotales :: [Ladron] -> Int
cantidadDeArmasTotales = length . map armas


-- 8) Hacer que los rehenes se rebelen

rebelarseContra :: [Rehen] -> Ladron -> Ladron
rebelarseContra unosRehenes unLadron = 
    foldl rebelarse unLadron (reducirEn10ElComplot unosRehenes)

rebelarse :: Ladron -> Rehen -> Ladron
rebelarse unLadron unRehen
    | esSubversivo unRehen = (plan unRehen) unLadron
    | otherwise          = unLadron

esSubversivo :: Rehen -> Bool
esSubversivo unRehen = nivelComplot unRehen > nivelMiedo unRehen

reducirEn10ElComplot :: [Rehen] -> [Rehen]
reducirEn10ElComplot = map (mapComplot (subtract 10))


-- 9) Ejecutar plan valencia

planValencia :: [Rehen] -> [Ladron] -> Int
planValencia unosRehenes = 
    (*1000000) . cantidadDeArmasTotales . rebelarseContraLadrones unosRehenes . armarLadronesConAmetralladoras 


armarLadronesConAmetralladoras :: [Ladron] -> [Ladron]
armarLadronesConAmetralladoras losLadrones =
    map (mapArmas (ametralladora 45 :)) losLadrones

rebelarseContraLadrones :: [Rehen] -> [Ladron] -> [Ladron]
rebelarseContraLadrones unosRehenes unosLadrones =
    map (rebelarseContra unosRehenes) unosLadrones

-- 10)
{-
¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de armas? Justifique.

No se puede ejecutar el Plan Valencia ya que es necesario saber la cantidad de armas que tienen los ladrones
Como la cantidad es infinita nunca va a terminar de contar y el programa se colgaria

-}

-- 11)
{-
¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de habilidades? Justifique.

En caso de que el plan valencia ejecute la funcion "esconderse", esto no va a ser posible ya que 
pasaria lo mismo que en el punto anterior, al evaluar constantemente las habilidades

En caso de que el plan valencia no ejecute le funcion "esconderse", no se evaluaran las habilidades,
entonces se podria ejecutar tranquilamente
-}