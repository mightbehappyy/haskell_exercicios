-- Lista 5

-- questão 1


impares [x] = [x]
impares (x:x':xs)
        | even x = impares (x':xs)
        | x < x' = x : impares (x':xs)
        | otherwise = x' : impares (x:xs)


aux' xs i
        | i == tamanho xs = xs
        | otherwise = aux' (impares xs) (i+1)

aux'' [xs] = [xs]
aux'' xs = aux' xs 0

-- questão 2


pos :: Int -> [a] -> a
pos n l = l !! n 


-- questão 3
repete n = [n | x <- [1..n]]
ajuda w = [repete x | x <- [w, w-1..1]]
merg [] = []
merg (x:xs) = x ++ head xs ++ merg xs


-- questão 4

palindromo l = reverse l == l 


-- questão 5

fibb 0 = 0
fibb 1 = 1
fibb n = fibb(n-1) + fibb(n-2)
fibonacci x = [fibb x| x <- [0..x-1]]
