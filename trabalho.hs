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


-- umas viagem que eu tive hj que acho q fazem sentido

separalinhas :: String -> [String]
length( takeWhile (/= '\n') s) == 0 = [] -- a base da recursao é essa, agr só falta completar ela



teste:: Int->String->String->[String]
teste 0 a b = []
teste x a b = a:b : teste (x-1) a b






aux1 :: Int->String->String
aux1 x s = auxseparalinhas 0 x s

auxseparalinhas :: Int->Int->String->String
auxseparalinhas y x [] = []
auxseparalinhas y x (a:z)
        |y<x = auxseparalinhas (y+1) x z
        |otherwise = a: (auxseparalinhas (y+1) x z) 




