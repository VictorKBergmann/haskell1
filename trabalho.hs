criaEspaso :: (String,Int) -> Int -> String
criaEspaso ( ( a : z ) ,b ) x 
        | [a] == ""            = ""
        | b == x                = ""
        | x - b > 0 && a /= ' ' = [a] ++ criaEspaso (z,b+1) (x-1)
        | otherwise             = [a] ++ " " ++ criaEspaso (z,b+1) (x-1)
         --x - b > 0 && a == ' '

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
