
module Opdracht2 where 

import Data.Char

-- ggd (grootste gemene deler) recursief. 
euclid::Integer->Integer->Integer
euclid x y = if y == 0 then x else euclid y (x `mod` y)

egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (b `mod` a) a
    in (g, t - (b `div` a) * s, s)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

egcd2 :: Integer -> Integer -> (Integer,Integer,Integer)
egcd2 a b  
    | snd3 r < 1 = (fst3 r, (snd3 r)+b, trd3 r)
    | otherwise = r
    where r = egcd a b

--priemgetallen voor sleutel
p = 439
q = 211

--de modulus
m = p * q

--eulers totient
m' = (p - 1)*(q - 1)

--lijst met mogelijke sleutels (pick one, i guess)
keylist = [x | x<-[0..m'], (euclid x m') == 1]

--publieke sleutel d van gekozen sleutel e.
keyd x = snd3 (egcd2 x m')

e = 90877
d = keyd e

--we hebben nu:
--p & q, de twee priemgetallen
--m, de modulus
--m', eulers totient
--keylist, alle mogelijke sleutels met deze priemgetallen
--private key e
--public key d

rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (key,md) x = x^key `mod` md

rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (key,md) x = x^key `mod` md

rsaencryptchar::(Integer,Integer)->Char->Integer
rsaencryptchar (key,md) x = rsaencrypt (key,md) (toInteger(ord x))

rsadecryptchar::(Integer,Integer)->Integer->Char
rsadecryptchar (key,md) x = chr (fromInteger (rsadecrypt (key,md) x))

--alice en bob zijn dom, want versleutelen
--doe je met een publieke sleutel, niet met 
--je private key.

--Hoe het wel moet:
--je stuurt je partner jouw publieke sleutel op,
--je partner encrypt met jouw publieke sleutel
--jij decrypt het met jouw privesleutel
--als jij antwoord wilt geven, moet je je partner
--een sleutelpaar laten genereren en jij de publieke
--sleutel ontvangen
