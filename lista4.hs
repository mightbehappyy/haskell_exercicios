-- primeira questão
fat :: (Eq p, Num p) => p -> p
fat 0 = 1
fat n = n * fat (n - 1)


-- segunda questão
somar 0 = 0
somar n = n + somar(n-1)

-- terceira questão

euclides :: Int -> Int -> Int
euclides x y
        | x == y = x
        | x > y = euclides (x-y) y
        | otherwise  = euclides x (y-x)


-- quinta questão

--a

listabla :: [Bool] -> Bool
listabla [] = True
listabla (x:xs)
        | not x = False
        | otherwise = listabla xs

--b

concatlistalista :: [[a]] -> [a]
concatlistalista [] = []
concatlistalista (x:xs) = x ++ concatlistalista xs


--c 
rep :: Int -> a -> [a]
rep 0 x = []
rep y x =  x : rep (y-1) x

--d 
sel :: [a] -> Int -> a

sel (x:xs) y
        | y /= 1 = sel xs (y-1)
        | otherwise = x

--e
listavalor :: Eq a => a -> [a] -> Bool
listavalor w [] = False
listavalor w (x:xs)
        | w == x = True
        | otherwise = listavalor w xs

-- questão 6

unir :: Ord a => [a] -> [a] -> [a]
unir [] ys = ys
unir xs [] = xs
unir (x:xs) (y:ys)
        | x < y = x : unir xs (y:ys)
        | otherwise  = y : unir (x:xs) ys
-- questão 7

-- a
auxsoma :: Num a => a -> a -> a
auxsoma x xs = x + xs
somaint [xs] = xs
somaint (x:x':xs) = auxsoma x x' + somaint xs


--b 
tamanho :: Num p => [a] -> p
tamanho [] = 0
tamanho (x:xs) = tamanho xs + 1

--c

ultimo :: [p] -> p
ultimo [x] = x
ultimo (x:xs) = ultimo xs
