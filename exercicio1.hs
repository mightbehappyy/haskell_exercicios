{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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



paresCons :: [b] -> [(b, b)]
paresCons (x:xs)= zip (x:xs) xs



isPrime :: Integral a => a -> Bool
isPrime n = length ([x | x<-[1..n], n`mod`x==0]) == 2

primos l = [x | x <- l, isPrime x]
