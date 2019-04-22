takewhile :: Char-> String->String
takewhile x [] = []
takewhile x (a:z)
        |a == x = []
        |otherwise = a : (takewhile x z)
