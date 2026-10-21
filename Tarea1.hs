-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Daniel Quijada
--  Carnet: 20-10518
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo (h:word)
  | not (isAlphaNum h)                    = esPalindromo word
  | esVacio word                          = True
  | not (isAlphaNum (last word))          = esPalindromo (h : init word)
  | (toLower h) == (toLower (last word))  = esPalindromo (init word)
  | otherwise                             = False
  

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- En el caso recursivo calcula el (max, min) de la cola de la lista,
-- y compara estos valores con la cabeza. El cálculo en la cola se hace
-- una sola vez y de acá se hacen las dos comparaciones necesarias
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (h:tl)
  | esVacio tl  = Just (h, h)
  | otherwise   = Just (max h maxNum, min h minNum)
  where values = fixPair (mayorYMenor tl)
        maxNum = fst values
        minNum = snd values

-- Auxiliar: fixPair
fixPair :: Maybe (a, a) -> (a, a)
fixPair (Just p)  = p
fixPair Nothing   = undefined


-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [h1, h2, ':', m1, m2]
  | not (isDigit h1)  = Left invalidFormat
  | not (isDigit h2)  = Left invalidFormat
  | not (isDigit m1)  = Left invalidFormat
  | not (isDigit m2)  = Left invalidFormat
  | hour > 23         = Left invalidHour
  | minute > 59       = Left invalidMin
  | otherwise         = Right (hour, minute)
  where hour            = (10*(digitToInt h1)) + (digitToInt h2)
        minute          = (10*(digitToInt m1)) + (digitToInt m2)
        invalidFormat   = "formato inválido"
        invalidHour     = "hora fuera de rango"
        invalidMin      = "minutos fuera de rango"

leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion t1 t2 =
  case leerHora t1 of
    Left str1      -> Left str1
    Right (h1, m1) -> case leerHora t2 of
                        Left str2               -> Left str2
                        Right (h2, m2) 
                          | 60*h2+m2 < 60*h1+m1 -> Right (1440 + (60*h2+m2) - (60*h1+m1))
                          | otherwise           -> Right ((60*h2+m2) - (60*h1+m1))

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral num lst
  | snd pair == 0 = Nothing
  | otherwise     = Just ((fst pair) / (snd pair))
  where pair = foldl (acumTuple num) (0.0, 0.0) lst

-- Auxiliar: acumTuple
acumTuple :: Double -> (Double, Double) -> Double -> (Double, Double)
acumTuple cond (sum, n) num
  | num > cond  = (sum + num, n + 1)
  | otherwise   = (sum, n)


-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (r, c)
  | r < 1     = []
  | r > 8     = []
  | c < 1     = []
  | c > 8     = []
  | otherwise = [(r+i, c+j) | i <- [-2, -1, 1, 2],
                              j <- [-2, -1, 1, 2],
                              abs(i) /= abs(j),
                              r+i > 0,
                              r+i < 9,
                              c+j > 0,
                              c+j < 9]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f = foldr (\val blst -> f val : blst) []

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter f = foldr (filterConcatVal f) []

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z x = foldr (\va fb vb -> fb (f vb va)) (\vb -> vb) x z

-- Auxiliar: filterConcatVal
filterConcatVal :: (a -> Bool) -> a -> [a] -> [a]
filterConcatVal f val lst
  | f val     = val : lst
  | otherwise = lst


-- =====================================================================
-- Funciones Auxiliares
-- Aquellas compartidas en dos o mas funciones
-- =====================================================================

-- esVacio
esVacio :: [a] -> Bool
esVacio []  = True
esVacio _   = False