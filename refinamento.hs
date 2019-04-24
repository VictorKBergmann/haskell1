separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas s = takeWhile (/= '\n') s : separaLinhas(auxseparaLinhas s)


auxseparaLinhas :: String->String
auxseparaLinhas s =  dropWhile (== '\n') (dropWhile (/= '\n') s)


tamanhoMaiorLinha :: [String]-> Int
tamanhoMaiorLinha s = maximum( [length(x)|x <-s])

