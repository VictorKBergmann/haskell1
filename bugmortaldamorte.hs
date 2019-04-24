texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para océu e tudo desde as chinelas\naté o céu tudo entra namesma sensação de propriedade."


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

insereEspacos :: Int-> String -> String
insereEspacos x s =  teste(x - (length s)) ++ s


teste :: Int->String
teste 0 = []
teste x = ' ' : teste (x-1)

justifica :: String -> String
justifica s = auxjustifica (separaLinhas s)

auxjustifica :: [String] ->String
auxjustifica [a] = a
auxjustifica (a:x) = (justificaLinha a (tamanhoMaiorLinha (a:x)) ) ++ auxjustifica x

justificaLinha:: String->Int->String
justificaLinha s n = auxjustfL(separaPalavras s) (length(separaPalavras s))     ((n  `div` (length(separaPalavras s) -1)))
										           				
auxjustfL::[String]->Int->Int->String
auxjustfL [] tamanho espacos = "\n"
auxjustfL (a:x) tamanho espacos
        |length((a:x)) == tamanho = espacoFinal a (mod (tamanho-1)  espacos) ++ (auxjustfL x tamanho espacos)
        |otherwise = (insereEspacos espacos a) ++ (auxjustfL x tamanho espacos)


 

espacoFinal:: String->Int->String
espacoFinal s n = s ++ teste n





