-- Primeira questão


somaQuadrados = sum [x * x | x<- [0..100]]


-- Segunda questão
grid :: Int -> Int -> [(Int,Int)]

grid n m = [(x,y) | x <-[0..n], y <-[0..m]]

--terceira questão
quadrado :: Int -> [(Int,Int)] 
quadrado n = [(x,y) | x <-[0..n], y <-[0..n], x /= y]

--quarta questão

replicar :: Int -> a -> [a]
replicar n m = [m | x<-[1..n]]

--quinta questão

verif x y z
        | (x*x) + (y*y) ==  (z*z) = True
        | otherwise = False
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | x <-[1..n], y <-[1..n], z <- [1..n],verif x y z]

--sexta questão


resto :: Integral a => a -> a
resto h = sum [x | x <- [1..h-1], h `mod` x == 0]

perfeito :: Integral a => a -> [a]
perfeito z = [x | x<- [1..z], resto x == x]


--setima questão

lista1 = [(x,y) | x <- [1,2], y <- [3,4]]


listaX = [x |x<-[1,2]]
listaY = [y |y<-[3,4]]

juntar = concat [[(x, y) | y <- listaY] | x <- listaX]
