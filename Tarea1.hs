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
  -- Si el primer caracter no es un caracter alfanumérico, se ignora y se compara el resto del String
  | not (isAlphaNum h)                    = esPalindromo word
  -- Si el resto del String es vacío, 'h' es un caracter central, por lo tanto, es un palíndromo
  | esVacio word                          = True
  -- Si el último caracter no es un caracter alfanumérico, se ignora y se compara el resto del String
  | not (isAlphaNum (last word))          = esPalindromo (h : init word)
  -- Si el primer y último caracter son iguales, se evalúan los caracteres en el medio
  | (toLower h) == (toLower (last word))  = esPalindromo (init word)
  -- En caso contrario, no es palíndromo
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
  | esVacio tl = Just (h, h)
  | otherwise = Just (max h maxNum, min h minNum)
  where values = fixPair (mayorYMenor tl)
        maxNum = fst values
        minNum = snd values


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
duracion t1 t2
  | isString t1Lect = Left (eitherToStr t1Lect)
  | isString t2Lect = Left (eitherToStr t2Lect)
  | t2Min < t1Min   = Right (1440 + t2Min - t1Min)
  | otherwise       = Right (t2Min - t1Min)
  where t1Lect  = leerHora t1
        t2Lect  = leerHora t2
        t1Pair  = eitherToPair t1Lect
        t2Pair  = eitherToPair t2Lect
        t1Min   = (60*(fst t1Pair)) + (snd t1Pair)
        t2Min   = (60*(fst t2Pair)) + (snd t2Pair)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined

-- =====================================================================
-- Funciones Auxiliares
-- =====================================================================

-- esVacio
esVacio :: [a] -> Bool
esVacio []  = True
esVacio _   = False

{-
-- mayorDeTres
mayorDeTres :: Ord a => a -> a -> a -> a
mayorDeTres a b c = max a (max b c)


-- menorDeTres
menorDeTres :: Ord a => a -> a -> a -> a
menorDeTres a b c = min a (min b c)
-}

-- fixPair
fixPair :: Maybe (a, a) -> (a, a)
fixPair (Just p)  = p
fixPair Nothing   = undefined


-- isString
isString :: Either String (Int, Int) -> Bool
isString (Left _)  = True
isString (Right _) = False


-- eitherToStr
eitherToStr :: Either String (Int, Int) -> String
eitherToStr (Left str)  = str
eitherToStr (Right _)   = ""


-- eitherToPair
eitherToPair :: Either String (Int, Int) -> (Int, Int)
eitherToPair (Right p)  = p
eitherToPair (Left _)   = (0, 0)