import Text.Show.Functions


-- Funciones ya definidas:

f1 :: Niveles -> Niveles
f1 (ns, nc, nf) = (ns + 1, nc + 2, nf + 3)

f2 :: Niveles -> Niveles
f2 (ns, nc, nf) = (ns, nc, nf + 5)

f3 :: Niveles -> Niveles
f3 (ns, nc, nf) = (ns, nc, nf - 3)

sinRepetidos [] = []
sinRepetidos (x:xs) = x : filter (/= x) (sinRepetidos xs)

-- misPociones = [felixFelices, multijugos]

invertir3 (a, b, c) = (c, b, a)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c


maxSegun f x y | f x > f y = x
               | otherwise = y

maximoSegun f xs = foldl1 (maxSegun f) xs 


-- Definir personas

data Persona = Persona {
    nombre :: String,
    nivelesDe :: Niveles
} deriving (Show, Eq)

data Pocion = Pocion {
    nombrePocion :: String,
    efectos :: Efectos,
    ingredientes :: [Ingrediente]
} deriving (Show)

type Efectos = [(Niveles->Niveles)]

type Niveles = (Int, Int, Int) -- (suerte, convencimiento, fisico)

type Ingrediente = (String, Int)

-- 1) Modelar personas

harry :: Persona
harry = Persona "Harry Potter" (11, 5, 4)

ron :: Persona
ron = Persona "Ron Weasley" (6, 4, 6)

hermione :: Persona
hermione = Persona "Hermione Granger" (12, 8, 2)


-- 2)

felixFelices :: Pocion
felixFelices = Pocion "felix felices" [f1, f2, f3] [("Escarabajos Machacados", 52), ("Ojo de Tigre Sucio", 2)]

multijugos :: Pocion
multijugos = Pocion "multijugos" [f3, f2, f1, invertir3, duplicarNiveles] [("Cuerno Bicornio en Polvo", 10), ("Sanguijuelas Hormonales", 54)]

mapNiveles :: (Niveles -> Niveles) -> Persona -> Persona
mapNiveles unaFuncion unaPersona = 
  unaPersona { nivelesDe = unaFuncion . nivelesDe $ unaPersona }

ejemploPociones :: [Pocion]
ejemploPociones = [felixFelices, multijugos]

duplicarNiveles :: Niveles -> Niveles
duplicarNiveles (ns, nc, nf) = (ns*2, nc*2, nf*2)


-- 3)

sumaNiveles :: Niveles -> Int
sumaNiveles = sum . listaDeNiveles

diferenciaNiveles :: Niveles -> Int
diferenciaNiveles unosNiveles = 
    maximum (listaDeNiveles unosNiveles) - minimum (listaDeNiveles unosNiveles)

listaDeNiveles :: Niveles -> [Int]
listaDeNiveles unosNiveles = 
    [fst3 unosNiveles, snd3 unosNiveles, trd3 unosNiveles]


-- 4)

sumaNivelesPersona :: Persona -> Int
sumaNivelesPersona = sumaNiveles . nivelesDe


diferenciaNivelesPersona :: Persona -> Int
diferenciaNivelesPersona = diferenciaNiveles . nivelesDe


-- 5)
efectosDePocion :: Pocion -> Efectos
efectosDePocion = efectos

-- 6) 
pocionesHeavies :: [Pocion] -> [Pocion]
pocionesHeavies = filter pocionTieneMasDe4Efectos

pocionTieneMasDe4Efectos :: Pocion -> Bool
pocionTieneMasDe4Efectos = (>4) . length . efectos



-- 7)

incluyeA :: Eq a => [a] -> [a] -> Bool
incluyeA lista1 lista2 = all (`elem` lista2) lista1

interseccionDeListas :: Eq a => [a] -> [a] -> [a]
interseccionDeListas lista1 lista2 =
  filter (\x -> any (x ==) lista2) lista1


-- 8)

esPocionMagica :: Pocion -> Bool
esPocionMagica unaPocion = 
    any (tieneTodasLasVocales) (ingredientes unaPocion) && all cantidadDeGramosEsPar (ingredientes unaPocion)

cantidadDeGramosEsPar :: (String, Int) -> Bool
cantidadDeGramosEsPar = even . snd

tieneTodasLasVocales :: (String, Int) -> Bool
tieneTodasLasVocales unIngrediente = incluyeA "aeiou" (fst unIngrediente) 

vocales :: [Char]
vocales = ['a','e','i','o','u']


-- 9)

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion unaPocion unaPersona = 
    mapNiveles (const $ aplicarEfectos (efectos unaPocion) unaPersona) unaPersona

aplicarEfectos :: Efectos -> Persona -> Niveles
aplicarEfectos losEfectos unaPersona = 
    foldl (\niveles efecto -> efecto niveles) (nivelesDe unaPersona) losEfectos


-- 10)

esAntidoto :: Persona -> Pocion -> Pocion -> Bool
esAntidoto unaPersona unaPocion otraPocion =
    (tomarPocion unaPocion unaPersona) == (tomarPocion otraPocion unaPersona)



-- 11)

personaMasAfectada :: Pocion -> (Niveles -> Int) -> [Persona] -> Persona -- Devuelve persona post efectos de pocion
personaMasAfectada unaPocion unaPonderacion unasPersonas =
    foldl1 (hayarElDeMayorPonderacion unaPocion unaPonderacion) unasPersonas 

hayarElDeMayorPonderacion :: Pocion -> (Niveles -> Int) -> Persona -> Persona -> Persona
hayarElDeMayorPonderacion unaPocion unaPonderacion x xs
  | unaPonderacion (nivelesDe postEfectoDelPrimero) > unaPonderacion (nivelesDe postEfectoDelSegundo) = x
  | otherwise                                                                                         = xs
  where postEfectoDelPrimero = tomarPocion unaPocion x
        postEfectoDelSegundo = tomarPocion unaPocion xs




-- 12)

{-

RESPUESTAS: Utilizo como ejemplo la pocion multijugos

a.
Main*> personaMasAfectada multijugos sumaNiveles [harry, ron, hermione]

b.
Main*> personaMasAfectada multijugos promedioNiveles [harry, ron, hermione]

c.
Main*> personaMasAfectada multijugos trd3 [harry, ron, hermione]

d.
Main*> personaMasAfectada multijugos diferenciaDeNiveles [harry, ron, hermione]

-}

masAfectadaPorSuma :: Pocion -> [Persona] -> Persona
masAfectadaPorSuma unaPocion unasPersonas = 
    personaMasAfectada unaPocion sumaNiveles unasPersonas

masAfectadaPorPromedio :: Pocion -> [Persona] -> Persona
masAfectadaPorPromedio unaPocion unasPersonas =
    personaMasAfectada unaPocion promedioNiveles unasPersonas

masAfectadaPorFuerza :: Pocion -> [Persona] -> Persona
masAfectadaPorFuerza unaPocion unasPersonas =
    personaMasAfectada unaPocion trd3 unasPersonas

masAfectadaPorDiferenciaDeNiveles :: Pocion -> [Persona] -> Persona
masAfectadaPorDiferenciaDeNiveles unaPocion unasPersonas =
    personaMasAfectada unaPocion diferenciaDeNiveles unasPersonas

-- Auxiliares
promedioNiveles :: Niveles -> Int
promedioNiveles niveles = (sumaNiveles niveles) `div` 3

diferenciaDeNiveles :: Niveles -> Int
diferenciaDeNiveles niveles = 
    maximum (listaDeNiveles niveles) - minimum (listaDeNiveles niveles)

-- 13)

superPocion :: [Ingrediente] -> Pocion
superPocion unosIngredientes = Pocion {
    nombrePocion = "Super Pocion",
    efectos = [], -- [(Niveles->Niveles)]
    ingredientes = (calcularIngredientes . map fst) $ unosIngredientes
}


calcularIngredientes :: [String] -> [Ingrediente]
calcularIngredientes unosNombres = 
    zipWith (\x y -> (x,y)) (cycle unosNombres) (enumFrom 1)