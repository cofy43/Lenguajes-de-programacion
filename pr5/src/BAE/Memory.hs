module BAE.Memory where

import BAE.Sintax as Sintax
import Data.List

newAddress :: Memory -> Expr
newAddress [] = (L 0)
newAddress (x:xs)
 | (checkMemory lista) = (L address)
 | otherwise = error "Corrupted memory."
 where lista = addressList (x:xs)
       address = newAddressAux lista 0 

--Funcion auxiliar que devuelve una lista de dirrecciones dad una memoria.
addressList :: Memory -> [Int]
addressList [] = []
addressList (x:xs) = (fst x) : (addressList xs)

--Funcion auxiliar que verifica que la memoria sea valida
checkMemory :: [Int] -> Bool
checkMemory [] = True
checkMemory (x:xs) 
 | (x `elem` xs) = False
 | otherwise = checkMemory xs

--Funcion auxiliar que devuelve una direccion de memoria
--dada una lista de enteros
newAddressAux :: [Int] -> Int -> Int
newAddressAux [] _ = 0
newAddressAux (x:xs) a
 | a `notElem` (x:xs) = a
 | otherwise = newAddressAux xs (a+1)

access :: Address -> Memory -> Maybe Value
access _ [] = Nothing
access a (x:xs)
 | (checkMemory lista) = if (length listExpr == 0) then Nothing else (Just (snd $ head listExpr))
 | otherwise = error "Corrupted memory."
 where listExpr = filter (\n -> (fst n == a)) (x:xs)
       lista = addressList (x:xs)

update :: Cell -> Memory -> Maybe Memory
update _ [] = Nothing
update cell (x:xs)
 | (checkMemory lista && checkCell cell) = memoria
 | otherwise = error "Memory can only store values."
 where lista = addressList (x:xs)
       memoria = updateAux cell (x:xs)

--Funcion auxiliar que verifica que una celda almacene un valor
checkCell :: Cell -> Bool
checkCell a = case snd a of
    (V x) -> True
    (I n) -> True
    (B b) -> True
    (Fn x e) -> True
    _ -> False

--Funcion auxiliar que actualiza el valor de una celda en la memoria
updateAux :: Cell -> Memory -> Maybe Memory
updateAux _ [] = Nothing
updateAux cell (x:xs)
 | (fst cell == fst x) = Just (cell : xs)
 | otherwise = updateAux cell xs

inMemory :: Maybe Memory -> Memory
inMemory Nothing = error "Cannot assing to empty memory"
inMemory (Just a) = a

inValue :: Maybe Value -> Value
inValue Nothing = error "Cannot fond the memory"
inValue (Just a) = a
