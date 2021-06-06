import Text.Show.Functions

-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int


-- 1a)
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = Tiro {
    velocidad = 10,
    precision = (precisionJugador unaHabilidad *2),
    altura = 0
}

madera :: Palo
madera unaHabilidad = Tiro {
    velocidad = 100,
    precision = div (precisionJugador unaHabilidad) 2,
    altura = 5
}

hierro :: Int -> Palo
hierro n unaHabilidad = Tiro {
    velocidad = fuerzaJugador unaHabilidad * n,
    precision = div (fuerzaJugador unaHabilidad) 2,
    altura = max 0 n-3
}


-- 1b)
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]


-- 2)

golpe :: Jugador -> Palo -> Tiro 
golpe unJugador unPalo = unPalo $ habilidad unJugador

-- Tambien puede definirse asi, pero en este caso habria que alterar los parametros de entrada
golpe' :: Palo -> Jugador -> Tiro 
golpe' unPalo = unPalo . habilidad 


-- 3)

type Obstaculo = Tiro -> Tiro

tiroNulo :: Tiro
tiroNulo = Tiro 0 0 0

-- a)
tunelConRampita :: Obstaculo
tunelConRampita unTiro
  | condicionTunel unTiro = efectoTunel unTiro
  | otherwise = tiroNulo

tunelConRampita' :: Obstaculo
tunelConRampita' = superaObstaculo condicionTunel efectoTunel

condicionTunel :: Tiro -> Bool
condicionTunel unTiro = precision unTiro > 90 && altura unTiro == 0

efectoTunel :: Tiro -> Tiro
efectoTunel unTiro = Tiro {
  velocidad = velocidad unTiro *2,
  precision = 100,
  altura = 0
}

-- b)
laguna :: Int -> Obstaculo
laguna largoLaguna unTiro
  | condicionLaguna unTiro = efectoLaguna largoLaguna unTiro
  | otherwise = tiroNulo

laguna' :: Int -> Obstaculo
laguna' largoLaguna = superaObstaculo condicionLaguna (efectoLaguna largoLaguna) 

condicionLaguna :: Tiro -> Bool
condicionLaguna unTiro = velocidad unTiro > 80 && 1 < altura unTiro && altura unTiro < 5

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoLaguna unTiro = unTiro {
  altura = (altura unTiro) `div` largoLaguna
}

-- c)
hoyo :: Obstaculo
hoyo = superaObstaculo condicionHoyo efectoHoyo

condicionHoyo :: Tiro -> Bool
condicionHoyo unTiro = 
  velocidad unTiro < 20 && 
  velocidad unTiro > 5 && 
  altura unTiro == 0 &&
  precision unTiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroNulo

-- Abstraccion usada en las funciones ' :
superaObstaculo :: (Tiro -> Bool) -> Obstaculo -> Tiro -> Tiro
superaObstaculo condicionObstaculo efectoObstaculo unTiro
  | condicionObstaculo unTiro = efectoObstaculo unTiro 
  | otherwise = tiroNulo


-- 4)

-- a)
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (esPaloUtil unObstaculo unJugador) palos

esPaloUtil :: Obstaculo -> Jugador -> Palo -> Bool
esPaloUtil unObstaculo unJugador = (esUtil unObstaculo) . (golpe unJugador)

esUtil :: Obstaculo -> Tiro -> Bool
esUtil unObstaculo = esTiroNulo . unObstaculo


-- b)


cantidadObstaculosConsecutivos :: [Obstaculo] -> Tiro -> Int
cantidadObstaculosConsecutivos [] _ = 0
cantidadObstaculosConsecutivos (x : xs) unTiro
  | puedeSuperarObstaculo x unTiro = 
    1 + cantidadObstaculosConsecutivos xs (efectoLuegoDeSuperar x unTiro)
  | otherwise = 0

efectoLuegoDeSuperar :: Obstaculo -> Tiro -> Tiro
efectoLuegoDeSuperar hoyo unTiro = efectoHoyo unTiro
efectoLuegoDeSuperar tunelConRampita' unTiro = efectoTunel unTiro

esTiroNulo :: Tiro -> Bool
esTiroNulo unTiro = unTiro /= tiroNulo

puedeSuperarObstaculo :: Obstaculo -> Tiro -> Bool
puedeSuperarObstaculo unObstaculo = esTiroNulo . unObstaculo

-- c)

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador unosObstaculos = 
  maximoSegun (cantidadObstaculosConsecutivos unosObstaculos . golpe unJugador) palos

-- Funciones útiles de la consigna
between n m x = elem x [n .. m]

maximoSegun f lista = foldl1 (mayorSegun f) lista

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- 5)
{-
Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos 
puntos ganó cada niño al finalizar el torneo, se pide retornar la lista de padres 
que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un 
niño ganó el torneo si tiene más puntos que los otros niños.

data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)
-}



type PuntajesJugadores = [(Jugador, Puntos)]

pierdenLaApuesta :: PuntajesJugadores -> [String]
pierdenLaApuesta puntosDeTorneo
  = map (padre.jugador) . filter (noGanoElTorneo puntosDeTorneo) $ puntosDeTorneo

-- Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

puntos = snd
jugador = fst

noGanoElTorneo :: PuntajesJugadores -> (Jugador,Puntos) -> Bool
noGanoElTorneo unosPuntajes = not . ganoElTorneo unosPuntajes 

ganoElTorneo :: PuntajesJugadores -> (Jugador,Puntos) -> Bool
ganoElTorneo unosPuntajes unJugador = 
  (== puntos unJugador) . maximum . (map puntos) $ unosPuntajes

puntajesDePrueba :: PuntajesJugadores
puntajesDePrueba = [(bart, 3000), (todd, 1000), (rafa, 1500)]
