module BAE.Type where

import Data.Set
import Data.List

type Identifier = Int
infix :->

data Type = T Identifier 
            | Integer | Boolean
            | Type :-> Type deriving (Show)

type Substitution = [(Identifier, Type)]

unique :: Ord a => [a] -> [a]
unique = toList . fromList

vars :: Type -> [Identifier]
vars (Integer) = []
vars (Boolean) = []
vars (T id) = [id]
vars (t1 :-> t2) = unique $ (vars t1) ++ (vars t2)

subst :: Type -> Substitution -> Type
subst Integer _ = Integer
subst Boolean _ = Boolean
subst (T id1) [] = (T id1)
subst (T id1) (x:xs)
    | (id1 == fst x) = (snd x)
    | otherwise = (subst (T id1) xs)
subst (t1 :-> t2) s = ((subst t1 s) :-> (subst t2 s))

leftSubst :: Substitution -> Substitution -> Substitution
leftSubst [] s = s
leftSubst s [] = s
leftSubst s1 s2 = (Data.List.map (\x -> (fst x, (subst (snd x) s2))) s1)

rightSubst :: Substitution -> Substitution -> Substitution
rightSubst [] s = s
rightSubst s [] = s
rightSubst s1 s2 = (Data.List.map (\x -> (fst x, (subst (snd x) s1))) s2)

comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = (leftSubst s1 s2) ++ (rightSubst s1 s2)

simpl :: Substitution -> Substitution
simpl [] = []
simpl ((a, (T id)):xs)
    | (a == id) = (simpl xs)
    | otherwise = ((a, (T id)):(simpl xs))
simpl (x:xs) = x:(simpl xs)