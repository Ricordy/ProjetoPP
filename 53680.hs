-- Função que devolve todas as cartas dum baralho
baralho :: [String]
baralho = [ valor ++ nipe 
          | valor <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"] -- Lista com todos os valores possiveis
          , nipe <- ["S","H","D","C"] -- Lista com todos os naipes possiveis
          ]

-- Função que devolve todas as combinações de 2 cartas que somadas tenham o valor enviado por parametro
combinacoesBlackjack :: Int ->  [(String,String)]
combinacoesBlackjack num = [ (v1 ++ n1,v2 ++ n2) -- Geração de pares
                           | v1 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"] -- Lista com todos os valores possiveis para 1 par
                           ,  v2 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"]-- Lista com todos os valores possiveis para 2 par
                           , n1 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 1 par
                           , n2 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 2 par
                           ,v1 <= v2 -- Retirar pares repetidos
                           , somaPares v1 v2 == num -- Verificas que a soma é o valor passado
                           ]
-- Função que devolve todas as combinações de 5 cartas que representem um full house
fullHouses :: [[String]]
fullHouses = [ [v1 ++ n1, v1 ++ n2, v1 ++ n3, v2 ++ n4, v2 ++ n5] -- Geração das combinações
             | v1 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"] -- Lista com todos os valores possiveis para 1 valor
             , v2 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"] -- Lista com todos os valores possiveis para 2 valor
             , n1 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 1 valor
             , n2 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 2 valor
             , n3 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 3 valor
             , n4 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 4 valor
             , n5 <- ["S","H","D","C"] -- Lista com todos os naipes possiveis para 5 valor
             , n1 < n2 -- Duas cartas com o mesmo valor não podem ter o mesmo naipe e retira-se os duplicados v1
             , n1 < n3 -- Duas cartas com o mesmo valor não podem ter o mesmo naipe e retira-se os duplicados v1
             , n2 < n3 -- Duas cartas com o mesmo valor não podem ter o mesmo naipe e retira-se os duplicados v1
             , n4 < n5 -- Duas cartas com o mesmo valor não podem ter o mesmo naipe e retira-se os duplicados v2
             , v1 /= v2 -- Os valores devem ser diferentes 
             ]
-- Função auxiliar que soma os valores das cartas 
somaPares:: String -> String -> Int
somaPares n1 n2 = valorCarta n1 + valorCarta n2 -- recorre à função valorCarta para obter os valores a somar 

-- Função auxiliar que retorna o valor da carta passada por parametro
valorCarta :: String -> Int
valorCarta (c:_) = case c of
    'A' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _   -> 10