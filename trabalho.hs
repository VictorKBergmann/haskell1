takewhile :: Char-> String->String
takewhile x [] = []
takewhile x (a:z)
        |a == x = []
        |otherwise = a : (takewhile x z)



tira :: [String] -> String
tira [] = ""
tira (t:x) = t

conta :: [[String]] -> [(String,Int)]
conta [] = []
conta (a:x) = ( tira a , length(a)) : (conta (x))


