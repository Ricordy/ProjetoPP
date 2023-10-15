baralho :: [String]
baralho = [ valor ++ nipe 
          | valor <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"]
          , nipe <- ["S","H","D","C"]
          ]


combinacoesBlackjack :: Int ->  [(String,String)]
combinacoesBlackjack num = [ (v1 ++ n1,v2 ++ n2) 
                           | v1 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"]
                           ,  v2 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"]
                           , n1 <- ["S","H","D","C"]
                           , n2 <- ["S","H","D","C"]
                           ,v1 <= v2
                           , somaPares v1 v2 == num 
                           ]

fullHouses :: [[String]]
fullHouses = [ [v1 ++ n1, v1 ++ n2, v1 ++ n3, v2 ++ n4, v2 ++ n5]
             | v1 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"]
             , v2 <- ["A","2","3","4","5","6","7","8","9","T","J","Q","K"]
             , n1 <- ["S","H","D","C"]
             , n2 <- ["S","H","D","C"]
             , n3 <- ["S","H","D","C"]
             , n4 <- ["S","H","D","C"]
             , n5 <- ["S","H","D","C"]
             , n1 /= n2
             , n1 /= n3
             , n2 /= n3
             , n4 /= n5
             , v1 /= v2
             ]
somaPares:: String -> String -> Int
somaPares n1 n2 = valorCarta n1 + valorCarta n2


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