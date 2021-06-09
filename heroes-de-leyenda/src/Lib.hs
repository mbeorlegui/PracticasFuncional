import Text.Show.Functions
import Data.List (nub)


-- 1) Modelado de tipos

data Heroe = Heroe {
  nombre :: String,
  epiteto :: String,
  reconocimiento :: Int,
  artefactos :: [Artefacto],
  tareas :: [Tarea]
} deriving (Show)

data Artefacto = Artefacto{
    nombreArtefacto :: String,
    rareza :: Int
} deriving (Show)

mapNombre :: (String-> String) -> Heroe -> Heroe
mapNombre unaFuncion unHeroe = unHeroe { nombre = unaFuncion . nombre $ unHeroe}

mapEpiteto :: (String->String) -> Heroe -> Heroe
mapEpiteto unaFuncion unHeroe = unHeroe { epiteto = unaFuncion . epiteto $ unHeroe}

mapReconocimiento :: (Int->Int) -> Heroe -> Heroe
mapReconocimiento  unaFuncion unHeroe = unHeroe { reconocimiento = unaFuncion . reconocimiento $ unHeroe}

mapArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefactos  unaFuncion unHeroe = unHeroe { artefactos = unaFuncion . artefactos $ unHeroe}

-- 2) Hacer que un heroe pase a la historia

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe 
    | suReconocimiento > 1000 = cambiarEpiteto "El mitico" unHeroe
    | suReconocimiento >= 500 = cambiarEpiteto "El magnifico" . agregarArtefacto lanzaDelOlimpo $ unHeroe
    | elReconocimiento >  100 = cambiarEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe
    | otherwise               = unHeroe
    where suReconocimiento = reconocimiento unHeroe -- Esta equivalencia solo existe dentro de la funcion
          elReconocimiento = suReconocimiento

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = mapArtefactos (unArtefacto :) unHeroe

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto unHeroe = mapEpiteto (const unEpiteto) unHeroe

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50


-- 3) Modelar tareas

type Tarea = Heroe -> Heroe

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto unHeroe = 
    agregarArtefacto unArtefacto . agregarReconocimiento (rareza unArtefacto) $ unHeroe

agregarReconocimiento :: Int -> Heroe -> Heroe
agregarReconocimiento unReconocimiento unHeroe =
    mapReconocimiento (+ unReconocimiento) unHeroe

escalarElOlimpo :: Tarea
escalarElOlimpo = 
    agregarReconocimiento 500 . mapArtefactos (desecharArtefactosMalos . triplicaRarezaArtefactos) . agregarArtefacto relampagoDeZeus 

desecharArtefactosMalos :: [Artefacto] -> [Artefacto]
desecharArtefactosMalos listaArtefactos = filter ((>=100).rareza) listaArtefactos


triplicaRarezaArtefactos :: [Artefacto] -> [Artefacto]
triplicaRarezaArtefactos listaArtefactos = map triplicaRareza listaArtefactos

triplicaRareza :: Artefacto -> Artefacto
triplicaRareza unArtefacto = unArtefacto { rareza = rareza unArtefacto * 3}

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "Relampago de Zeus" 500

-- 6)
realizarTarea :: Tarea -> Heroe -> Heroe
realizarTarea unaTarea unHeroe = agregarTarea unaTarea (unaTarea unHeroe)
--                                                     ^^^^^^^^^^^^^^^^^^
--                                                      Heroe post-tarea

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea unaTarea unHeroe = mapTarea (unaTarea :) (unHeroe)

mapTarea :: ([Tarea] -> [Tarea]) -> Heroe -> Heroe
mapTarea unaFuncion unHeroe = unHeroe { tareas = unaFuncion . tareas $ unHeroe}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadCuadras unHeroe = cambiarEpiteto ("Groso" ++ replicate cantidadCuadras '0') unHeroe

data Bestia = Bestia{
    nombreBestia :: String,
    debilidad :: Debilidad
}deriving (Show)

type Debilidad = Heroe -> Bool

matarA:: Bestia -> Tarea
matarA unaBestia unHeroe
    | puedeMatarA unaBestia unHeroe = cambiarEpiteto ("Asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise                     = cambiarEpiteto "El cobarde" . perderPrimerArtefacto $ unHeroe

perderPrimerArtefacto :: Heroe -> Heroe
perderPrimerArtefacto unHeroe = mapArtefactos (drop 1) unHeroe

puedeMatarA :: Bestia -> Heroe -> Bool
puedeMatarA unaBestia unHeroe = (debilidad unaBestia) unHeroe


-- 4) Modelar a Heracles

heracles :: Heroe
heracles = Heroe "Heracles" "Guardial del olimpo" 700 [pistola, relampagoDeZeus] [matarAlLeonDeNemea]

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" debilidadDeLeonDeNemea

debilidadDeLeonDeNemea :: Debilidad
debilidadDeLeonDeNemea unHeroe = (>=20) . length . epiteto $ unHeroe

-- 5) Modelar la tarea "Matar al leon de Nemea"

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarA leonDeNemea


-- 6) Hacer que un heroe HAGA una tarea (Esta mas arriba)

-- 7) Hacer que dos heroes presuman sus logros ante el otro

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir unHeroe otroHeroe
    | reconocimiento unHeroe > reconocimiento otroHeroe                                 = (unHeroe, otroHeroe)
    | reconocimiento otroHeroe > reconocimiento unHeroe                                 = (otroHeroe, unHeroe)
    | sumatoriaDeRarezasDeArtefactos unHeroe > sumatoriaDeRarezasDeArtefactos otroHeroe = (unHeroe, otroHeroe)
    | sumatoriaDeRarezasDeArtefactos unHeroe > sumatoriaDeRarezasDeArtefactos otroHeroe = (otroHeroe, unHeroe)
    | otherwise           = presumir (realizarTareasDe otroHeroe unHeroe) (realizarTareasDe unHeroe otroHeroe)

sumatoriaDeRarezasDeArtefactos :: Heroe -> Int
sumatoriaDeRarezasDeArtefactos = sum . map rareza . artefactos

realizarTareasDe :: Heroe -> Heroe -> Heroe
realizarTareasDe unHeroe otroHeroe = foldr realizarTarea otroHeroe (tareas unHeroe)

-- 8) Resultado de hacer que...
-- Queda culgado el programa en la funcion presumir ya que no deja de evaluarla

-- 9) Hacer que un heroe realice una labor
type Labor = [Tarea]

realizarLabor :: Labor -> Heroe -> Heroe
realizarLabor unaLabor unHeroe = foldr realizarTarea unHeroe unaLabor

descendientes :: Heroe -> [Heroe]
descendientes unHeroe = tail . iterate descendiente $ unHeroe

descendiente :: Heroe -> Heroe
descendiente unHeroe = realizarTareasDe unHeroe . mapArtefactos nub . mapNombre (++ "*") $ unHeroe

-- 10)

