import Text.Show.Functions
import Data.List

-- Dado un tipo de dato

data TipoDato = TipoDato {
    parametro :: Int
}

mapComidaFav :: (Int -> Int) -> TipoDato -> TipoDato 
mapComidaFav unaFuncion unDato = 
  unDato { parametro = unaFuncion . parametro $ unDato }

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = 
  (map efecto . filter criterio) lista ++ filter (not.criterio) lista