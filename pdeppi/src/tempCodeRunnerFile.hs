import Text.Show.Functions
import Data.List

-- Inicio: 19.36
data Persona = Persona {
    nombre :: String,
    direccion :: String,
    dinero :: Int,
    comidaFav :: Comida,
    cupones :: [String]
}

data Comida = Comida {
    nombreComida :: String,
    costo :: Int,
    ingredientes :: [String]
}