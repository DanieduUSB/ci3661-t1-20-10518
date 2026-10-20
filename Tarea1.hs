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
  -- Si el primer caracter es un espacio en blanco, se ignora y se compara el resto del String
  | h == ' '          = esPalindromo word
  -- Si el resto del String es vacío, 'h' es un caracter central, por lo tanto, es un palíndromo
  | esVacio word      = True
  -- Si el último caracter es un espacio en blanco, se ignora y se compara el resto del String
  | last word == ' '  = esPalindromo (h : init word)
  -- Si el primer y último caracter son iguales, se evalúan los caracteres en el medio
  | h == last word    = esPalindromo (init word)
  -- En caso contrario, no es palíndromo
  | otherwise         = False
  

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor = undefined

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

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