import Text.Show.Functions
import Data.List

data Auto = Auto {
  color     :: Color,
  velocidad :: Int,
  distancia :: Int
} deriving (Show, Eq)

type Color = String

type Carrera = [Auto]

type Evento = Carrera -> Carrera

autoEj1 :: Auto
autoEj1 = Auto {
  color = "rojo",
  velocidad = 100,
  distancia = 298
}

autoEj2 :: Auto
autoEj2 = Auto {
  color = "verde",
  velocidad = 150,
  distancia = 390
}

carrera1 :: Carrera
carrera1 = [autoEj2, autoEj2]

carrera2 :: Carrera
carrera2 = [autoEj1, autoEj1]
-- Mapeos
mapColor     :: (String -> String) -> Auto -> Auto 
mapColor unaFuncion auto = 
  auto { color = unaFuncion . color $ auto }

mapVelocidad :: (Int -> Int) -> Auto -> Auto 
mapVelocidad unaFuncion auto = 
  auto { velocidad = unaFuncion . velocidad $ auto }

mapDistancia :: (Int -> Int) -> Auto -> Auto 
mapDistancia unaFuncion auto = 
  auto { distancia = unaFuncion . distancia $ auto }

-- 1)

autosEstanCerca :: Auto -> Auto -> Bool
autosEstanCerca auto1 auto2 = (<10) $ abs (subtract (distancia auto1) (distancia auto2))


-- Teniendo en cuenta que el Auto ingresado no forma parte de la carrera
autoVaTranquilo :: Auto -> Carrera -> Bool
autoVaTranquilo auto carrera = (&&) (noHayAutosCercaEnCarrera auto carrera) (autoVaPrimero auto carrera)

noHayAutosCercaEnCarrera :: Auto -> Carrera -> Bool
noHayAutosCercaEnCarrera auto carrera = any (not . autosEstanCerca auto) (excluirAutoDeCarrera auto carrera)

excluirAutoDeCarrera :: Auto -> Carrera -> Carrera
excluirAutoDeCarrera auto carrera = filter (==auto) (carrera)

autoVaPrimero :: Auto -> Carrera -> Bool
autoVaPrimero auto carrera = any ((>) (distancia auto) . distancia) (excluirAutoDeCarrera auto carrera)

-- Puesto del auto
puestoDelAuto :: Auto -> Carrera -> Int
puestoDelAuto unAuto = (+1) . (cantidadDeAutosAdelante unAuto)

cantidadDeAutosAdelante :: Auto -> Carrera -> Int 
cantidadDeAutosAdelante unAuto = length . autosQueVanAdelante unAuto

autosQueVanAdelante :: Auto -> Carrera -> Carrera
autosQueVanAdelante unAuto = filter (autoVaAdelante unAuto) 

autoVaAdelante :: Auto -> Auto -> Bool
autoVaAdelante unAuto = (<) (distancia unAuto) . distancia

-- 2)

{-
  a)
  Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo
  indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba
  recorrida + ese tiempo * la velocidad a la que estaba yendo.
-}

correr :: Int -> Auto -> Auto
correr tiempo auto = mapDistancia (const (tiempo * (velocidad auto) + (distancia auto))) auto

{-
  b)
  - A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un
  auto de modo que su velocidad final sea la resultante de usar dicho modificador con su
  velocidad actual
-}

mapVelocidad' :: (Int -> Int) -> Auto -> Auto 
mapVelocidad' unaFuncion auto = 
  auto { velocidad = unaFuncion . velocidad $ auto }

{-
  Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad
  indicada de modo que se le reste a la velocidad actual la cantidad indicada, y como
  mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.
-}

reducirVelocidadEn :: Int -> Auto -> Auto
reducirVelocidadEn reductor auto
  | (velocidad auto) - reductor >= 0 = mapVelocidad' (subtract reductor) auto
  | otherwise                        = mapVelocidad' (const 0)           auto



-- 3) Power Ups

-- Funcion de ayuda
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = 
  (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> Carrera -> Carrera
terremoto unAuto = 
  afectarALosQueCumplen (autosEstanCerca unAuto) (reducirVelocidadEn 50)

miguelitos :: Int -> Auto -> Carrera -> Carrera
miguelitos reductor unAuto = 
  afectarALosQueCumplen (autoVaAdelante unAuto) (reducirVelocidadEn reductor)

jetPack :: Int -> Auto -> Auto
jetPack duracion = (mapVelocidad' (`div` 2)) . (correr duracion) . (mapVelocidad' (*2))

-- 4) Simular carrera

-- simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
-- simularCarrera unaCarrera unosEventos = aplicarEventosACarrera unaCarrera unosEventos

hayarPosicionesYColor :: Carrera -> [(Int, Color)]
hayarPosicionesYColor []    = []
hayarPosicionesYColor (x:xs) = (puestoDelAuto x xs, color x) : hayarPosicionesYColor xs

aplicarEventosACarrera :: Carrera -> [Evento] -> Carrera
aplicarEventosACarrera unaCarrera unosEventos =
  foldl (\carrera evento -> evento carrera) unaCarrera unosEventos
