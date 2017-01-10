module Opdracht1

where

faca :: Int -> Int
faca 0 = 1
faca x = x * faca(x-1)

facb :: Int -> Int
facb x
    | x == 0 = 1
    | otherwise = x * facb (x-1)

--bereken D in de ABC formule
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = (b^2)-(4*a*c)

--vindt de nulpunten van een 2e graadsfunctie
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c
    | discriminant a b c < 0 = []
    | discriminant a b c == 0 = [ b / ( 2*a )]
    | otherwise  = [( b-sqrt( discriminant a b c ) ) / ( 2*a ), ( b+sqrt( discriminant a b c ) )/( 2*a )]


--vindt de nulpunten van een 2e graadsfunc tie
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
    | d < 0 = []
    | d == 0 = [ b / ( 2*a )]
    | otherwise = [( b-sqrt d ) / ( 2*a ), ( b + sqrt d )/ ( 2*a )]
    where d = discriminant a b c

--Dobbel combinaties waar de som een veelvoud van 5 is
dobbela = [(a,b,c) | a <- [1..6], b <- [1..6], c <- [1..6], (a+b+c) `mod` 5 == 0]

--Dobbel combinaties waar de som een veelvoud van x is
dobbelb x = [(a,b,c) | a <- [1..6], b <- [1..6], c <- [1..6], (a+b+c) `mod` x == 0]