module BAE.Static where

import BAE.Type as Type
import BAE.Sintax as Sintax

type Ctxt = [(Sintax.Identifier, Type.Type)]
type Constraint = [(Type.Type, Type.Type)]

substAux :: (Sintax.Identifier, Type.Type) -> Substitution -> (Sintax.Identifier, Type.Type)
substAux x [] = x
substAux (id1, (Type.T id2)) ((a, t):xs)
    | (id2 == a) = (id1, t)
    | otherwise = (substAux (id1, (Type.T id2)) xs) 

subst :: Ctxt -> Substitution -> Ctxt
subst ctx s = (map (\x -> (substAux x s)) ctx)

find :: Sintax.Identifier -> Ctxt -> Maybe Type.Type
find a [] = Nothing
find a ((x, t):xs)
    | (a == x) = Just t
    | otherwise = find a xs

typeToInt :: Type.Type -> Int
typeToInt (Type.T a) = a
typeToInt _ = error "Cannot extract identifier from non type variable"

typesToInts :: [Type.Type] -> [Int]
typesToInts types = (map typeToInt types)

fresh :: [Type.Type] -> Type.Type
fresh [] = (Type.T 1)
fresh s = (Type.T ((maximum (typesToInts s)) + 1))