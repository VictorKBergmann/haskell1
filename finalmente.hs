texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para océu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade."


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
insereEspacos x s =  teste x ++ s


teste :: Int->String
teste 0 = []
teste x = ' ' : teste (x-1)

justifica :: String -> String
justifica s = auxjustifica (separaLinhas s) (tamanhoMaiorLinha(separaLinhas s))

auxjustifica :: [String]->Int->String
auxjustifica [a] n = a ++ "\n"
auxjustifica (a:x) n = (justificaLinha a n) ++ "\n"  ++ auxjustifica x n

justificaLinha:: String->Int->String
justificaLinha s n = var1 ++ auxjustfL (resto ( (n - var3) `mod` (length(var2))) (var2)      )  ((n - var3)`div` (length(var2)))   
	where 
		var1 = head(separaPalavras s)
		var2 = tail(separaPalavras s)				
		var3 = length(s)
										           				
auxjustfL::[String]->Int->String
auxjustfL [a] qtd = (insereEspacos (qtd+1) a)
auxjustfL (a:x) qtd = (insereEspacos (qtd+1) a) ++ auxjustfL x qtd  

resto::Int->[String]->[String]
resto 0 s = s
resto n (a:x) = (insereEspacos 1 a) : resto (n-1) x 















