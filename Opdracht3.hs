module Opdracht3 where 

import Data.List

-- Willekeurige tweedegraadsfunctie om differentieer en integreer functies te testen
func1::Double->Double
func1 x = x^2 + x + 10

--differentiequotient functie
differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = ((f (x-p)) - (f x)) / p * (-1)

--Riemannintegratie functie
integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p = sum [(f a)*p, (f (a+p))*p..(f b)*p]

--haalt dubbelen uit een lijst
dubbelen::(Eq a)=>[a]->[a]
dubbelen s 
    | s == [] = []
    | otherwise = nub (s\\(nub s))


worpen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s] where s = [1..6]

paren::(Eq a)=>[a]->Int
paren x = length (dubbelen x)

count::Integer->[Integer]->Integer
count c [] = 0
count c (x:xs)
    |c==x= 1 + (count c xs)
    |otherwise = count c xs

convert list = ([a,b,c,d,e,f],list) where
    a = count 1 list
    b = count 2 list
    c = count 3 list
    d = count 4 list
    e = count 5 list
    f = count 6 list

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

maxDuplicate::[Integer]->Integer
maxDuplicate x = maximum (fst (convert x))

sndDuplicate::[Integer]->Integer
sndDuplicate x = reverse (sort (fst (convert x))) !! 1

poker::[Integer]->Bool
poker x = maxDuplicate x == 5

foak::[Integer]->Bool
foak x = maxDuplicate x == 4

toak::[Integer]->Bool
toak x = (maxDuplicate x == 3) && (sndDuplicate x /= 2)

fullhouse::[Integer]->Bool
fullhouse x = (maxDuplicate x == 3) && (sndDuplicate x == 2)

twopair::[Integer]->Bool
twopair x = (maxDuplicate x == 2) && (sndDuplicate x == 2)

onepair::[Integer]->Bool
onepair x = (maxDuplicate x == 2) && (sndDuplicate x == 1)
 
straight::[Integer]->Bool
straight x = ((dubbelen x) == []) && (xor (elem 1 x) (elem 6 x))


{-
Gebruik:
$ filter poker worpen
Geeft alle pokers uit de lijst worpen
-}



    
