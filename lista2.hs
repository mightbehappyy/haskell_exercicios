--Primeira questão

soma n1 n2 = sum (replicate n1 n2)

-- Segunda questão

raiz 0 = 0
raiz x  = sqrt (6 + raiz (x-1))

-- Terceira questão


maior [x] = x
maior (x:x':xs)
        | x >= x' = maior (x:xs)
        | otherwise = maior (x':xs)

listanum l = maior(zip l [1..])


-- quarta questão

dic10 = ["zero", "um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove"]
traduz x = snd (zip [0..9] dic10 !! x)

-- quinta questão
removerNum _ [] = []
removerNum y (x:xs)
        | y == x    = removerNum y xs
        | otherwise = y : removerNum y xs

-- sexta questão
adcPosicaoN (x:xs) 0 p = p:x:xs
adcPosicaoN (x:xs) n p  = x: adcPosicaoN xs (n-1) p



-- Setima questão
delPosicaoN :: [Int] -> Int -> [Int]
delPosicaoN (x:xs) 0 = [x]
delPosicaoN (x:xs) n = delPosicaoN xs (n-1)
