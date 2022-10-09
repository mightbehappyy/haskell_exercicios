-- primeira questão
fat :: (Eq p, Num p) => p -> p
fat 0 = 1
fat n = n * fat (n - 1)


-- segunda questão
somar 0 = 0
somar n = n + somar(n-1)

