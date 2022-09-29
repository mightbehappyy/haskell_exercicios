{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (COORD(x))
{-# HLINT ignore "Use camelCase" #-}

fatorial n = foldr (+) 0 [1..n]

aux 0 = 0
aux 1 = 1
aux n = aux (n-1) + aux(n-2)

fib n = [aux n| n <- [1..n]]

tres n = [x | x <- [0,3..n]]

mult23 = [x | x <- [1..20], mod x 2 == 0 || mod x 3 == 0]

listalista =[[n] | n <- [1..5]]

repetir = [replicate n  1 | n <- [1..5]]

matrix  = [(x,y) | x <- [1..3], y <- [3,2..1]]

inserirord n [] = [n]
inserirord n (x:xs)
        | n <= x = n : x: xs
        | otherwise = x : inserirord n xs

insere n l
        | elem n l == False = inserirord n l
        | otherwise = l

retornaSup n (x:xs)
        | n < x = x:xs
        | otherwise = retornaSup n xs

retornaSup2 n (x:xs) = [x | x <- x:xs, n < x]

misterio :: String -> String
misterio p = [c | c<-p, c>='a' && c<='z']

paresCons :: [b] -> [(b, b)]
paresCons (x:xs)= zip (x:xs) xs

isPrime :: Integral a => a -> Bool
isPrime n = length ([x | x<-[1..n], n`mod`x==0]) == 2

primos l = [x | x <- l, isPrime x]
-- Primeira questão
valores x y z
        | x == y && x == z && y == z = "Todos iguais"
        | x /= y && x /= z && y /= z = "Todos diferentes"
        | otherwise = "Dois iguais"
-- Segunda questão
media x y z
        | x > (x+y+z)/3 && y > (x+y+z)/3  || z > (x+y+z)/3 && y > (x+y+z)/3 || x > (x+y+z)/3 && z > (x+y+z)/3  = "2 valores maior que a media"
        | x == (x+y+z)/3 && y == (x+y+z)/3  && z == (x+y+z)/3 = "Nenhum valor maior que a media"
        | otherwise = "1 valor maior que a media"
--Terceira questão
potencia_2 x = x * x
--Quarta questão
potencia_4 x = potencia_2 x * potencia_2 x
--Quinta questão
ouExclusivo a b
        |b /= a = True
        |otherwise = False
--Sexta questão
delta a b c = (b * b) - (4 * a * c)
raizDelta a b c = sqrt ((b^2) - (4 * a * c))
x_Maior a b c
        | ((b^2) - (4 * a * c))  < 0 = 0
        | otherwise = (-b + raizDelta a b c)/ (2 * a)

x_Menor a b c
        | ((b^2) - (4 * a * c))  < 0 = 0
        | otherwise = (-b - raizDelta a b c)/ (2 * a)
--Setima questão
somaComLimite n1 n2 = sum [x | x <- [n1..n2]]

somaSemLimite n1 n2 = sum [x | x <- [n1+1..n2-1]]
--Oitava questão
mutiplosIntervalo n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

--Nona questão
fato x = product [x |x <- [1..x]]

ordem m n = (fato m) / (fato n * fato (m-n))


--Decima questão
mod2 x y 
        | x < y = x
        | otherwise = mod2(x - y) y
