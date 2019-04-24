separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas s = takeWhile (/= '\n') s : separaLinhas(auxseparaLinhas s)


auxseparaLinhas :: String->String
auxseparaLinhas s =  dropWhile (== '\n') (dropWhile (/= '\n') s)


tamanhoMaiorLinha :: [String]-> Int
tamanhoMaiorLinha s = maximum( [length(x)|x <-s])

separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras s = rmvpalavrasVazias ( takeWhile (/= ' ') s : separaPalavras(auxseparaPalavras s)   )


auxseparaPalavras :: String->String
auxseparaPalavras s =  dropWhile (== ' ') (dropWhile (/= ' ') s)

rmvpalavrasVazias :: [String] ->[String]   -- usado no separaPalavras
rmvpalavrasVazias [] = []
rmvpalavrasVazias (a:x)
        |a /= "" = a: rmvpalavrasVazias x
        |otherwise = rmvpalavrasVazias x 

insereEspacos :: Int -> String -> String
insereEspacos x s = (teste x) ++ s


teste :: Int->String
teste 0 = []
teste x = ' ' : teste (x-1)




justifica :: String -> String
justifica s = auxjustifica (separaLinhas s)

auxjustifica :: [String] ->String
auxjustifica [a] = a
auxjustifica (a:x) = (justificaLinha a (tamanhoMaiorLinha (a:x))) ++ auxjustifica x

justificaLinha:: String->Int->String
justificaLinha s n = auxjustLinha (separaPalavras s) n

auxjustLinha:: [String]->String




junta :: [String] -> String
junta [] = ""
junta ( a : x ) = a ++ junta x 

conta :: [[String]] -> [(String,Int)]
conta [] = []
conta (a:x) = ( tira a , length(tira a)) : (conta (x))

tira :: [String] -> String
tira [] = ""
tira (t:x) = t

--a partir daqui, nn tenho certeza de nada

justifica :: String -> String
justifica a = junta( criaEspaso ( conta( ( separalinhas a ) ) ) , tamanhoMaiorLinha a  )

-- criaespaso ta todo errado, pq eu ia comesar a fazer ele em listcompresion agr
criaEspaso :: [(String,Int)] -> Int -> ["linha","linha"]
criaEspaso [ ( ( a : z ) ,b ) x 
        | [a] == [] && x /= b   =  
        | b == x                =  a:z
        | x - b > 0 && a /= ' ' = [a] ++ criaEspaso (z,b) (x)
        | otherwise             = [a] ++ " " ++ criaEspaso (z,b+1) (x)
--x - b > 0 && a == ' '
