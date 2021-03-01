-- 50 Questões para o 1º teste de Programação Funcional

-- 1 Constroi a lista dos numeros inteiros compreendidos entre dois limites.

enumFromToRec ::  Int -> Int ->[Int]
enumFromToRec a b | a==b = [a]
                  | a<b = a : enumFromToRec (a+1) b
                  | otherwise = []


-- 2 Constroi a lista dos numeros inteiros compreendidos entre dois limites e espaçados de um valor constante.

enumFromThenToRec :: Int -> Int -> Int -> [Int]
enumFromThenToRec a x b | a>b = []
                        | a==b = [a]
                        | otherwise = a : enumFromThenToRec x (2*x - a) b

-- 3 Concatena duas listas

(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (x:xs) l = x : ((+++) xs l)

-- 4 Dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posiçao (assume-se que o primeiro elemento se encontra na posiçao 0).

(!!!)  :: [a] -> Int -> a
(!!!) (h:t) a | a==0 = h
              | otherwise = (!!!) t (a-1)

-- 5 Dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.

reverseRec ::  [a] -> [a]
reverseRec [x] = [x]
reverseRec (h:t) = (reverseRec t) ++ [h]

-- 6 Dado um inteiro n e uma lista l calcula a lista com os (no maximo) n primeiros elementos de l.

takeRec :: Int -> [a] -> [a]
takeRec 0 (h:t) = []
takeRec _ [] = [] 
takeRec a (h:t) = if a>0 then h : (takeRec (a-1) t) else []

-- 7 Dado um inteiro n e uma lista l calcula a lista sem os (no maximo) n primeiros elementos de l.

dropRec :: Int -> [a] -> [a]
dropRec 0 l = l
dropRec n [] = [] 
dropRec n (h:t) = dropRec (n-1) t 

-- 8 Constroi uma lista de pares a partir de duas listas.

zipRec :: [a] -> [b] -> [(a,b)]
zipRec [] (x:xs) = []
zipRec (y:ys) [] = []
zipRec (x:xs) (y:ys) = (x,y) : (zipRec xs ys)

-- 9 Testa se um elemento ocorre numa lista.

elemRec :: Eq a => a -> [a] ->Bool
elemRec a (h:t) = if a /= h then elem a t else True

-- 10 Dado um inteiro n e um elemento x constroi uma lista com n elementos, todos iguais a x.

replicateRec :: Int -> a ->[a]
replicateRec 0 x = []
replicateRec n x = x : (replicateRec (n-1) x)

-- 11 Dado  um  elemento  e  uma  lista,  constroi  uma  lista  em  que  o  elemento  fornecido  ́e intercalado entre os elementos da lista fornecida.

intersperseRec :: a -> [a] ->[a]
intersperseRec x [] = []
intersperseRec _ [x] = [x]
intersperseRec x (h:t) = h : x : intersperseRec x t

-- 12 Agrupa elementos iguais e consecutivos de uma lista
{-
groupRec :: Eq a => [a] -> [[a]]
groupRec x = [x]
groupRec (x:y:t) = if x==y then (x:y) : groupRec y:z else [x] : groupRec (y:t)
-}

-- 13 Concatena as listas de uma lista.

concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (h:t) = h ++ (concatRec t)

-- 14 Calcula a lista dos prefixos de uma lista.

initsRec ::  [a] -> [[a]]
initsRec [] = [[]]
initsRec l = initsRec (init l) ++ [l] 

-- 15 Calcula a lista dos sufixos de uma lista.

tailsRec ::  [a] -> [[a]]
tailsRec [] = [[]]
tailsRec l = l : tailsRec (tail l)

-- 16 Testa se uma lista é prefixo de outra.

isPrefixOfRec :: Eq a => [a] -> [a] -> Bool 
isPrefixOfRec [] _ = True 
isPrefixOfRec _ [] = False 
isPrefixOfRec (x:xs) (y:ys) = x==y && isPrefixOfRec xs ys 
--isPrefixOfRec (x:y) (h:t) = if x==h then isPrefixOfRec y t else False  -- OUTRO MODO

-- 17 Testa se uma lista ́e sufixo de outra.

isSuffixOfRec :: Eq a => [a] -> [a] -> Bool
isSuffixOfRec [] _ = True 
isSuffixOfRec _ [] = False 
isSuffixOfRec l1 l2 = last l1 == last l2 && isSuffixOfRec (init l1) (init l2)

-- 18 Testa  se  os  elementos  de  uma  lista  ocorrem  noutra  pela  mesma ordem relativa.

isSubsequenceOfRec :: Eq a => [a] -> [a] -> Bool
isSubsequenceOfRec [] _ = True 
isSubsequenceOfRec _ [] = False 
isSubsequenceOfRec (x:xs) (y:ys) | x /= y = isSubsequenceOfRec (x:xs) ys 
                                 | x == y = isSubsequenceOfRec xs ys 
                                 | otherwise = False 

-- 19 Calcula a lista de posiçes em que um dado elemento ocorre numa lista.

elemIndicesRec ::  Eq a => a ->[a] -> [Int]
elemIndicesRec _ [] = []
elemIndicesRec n l  = elemIndicesRecAux 0 n l

elemIndicesRecAux :: Eq a => Int -> a -> [a] -> [Int] -- x é a posiçao, n o numero a procurar em (h:t)
elemIndicesRecAux x _ [] = []
elemIndicesRecAux x n (h:t) | n == h = x : elemIndicesRecAux (x+1) n t 
                            | n /= h = elemIndicesRecAux (x+1) n t

-- 20 Calcula uma lista com os mesmos elementos da recebida, sem repetiçoes.

nubRec ::  Eq a => [a] -> [a]
nubRec [] = []
nubRec (x:xs) = if elem x xs then nubRec xs else x:nubRec xs 

-- 21 Retorna a lista resultante de remover (a primeira ocorência) de um dado elemento de uma lista.

deleteRec ::  Eq a => a -> [a]-> [a]
deleteRec n [] = []
deleteRec n (h:t) = if n==h then t else h:deleteRec n t  

-- 22 Retorna a lista resultante de remover (as primeiras ocorrencias) dos elementos da segunda lista da primeira.

(\\\) ::  Eq a => [a] -> [a]-> [a]
(\\\) [] l = []
(\\\) l [] = l
(\\\) (x:xs) (h:t) = if x==h then (\\\) xs t  else x : (\\\) xs (h:t)

-- 23 Retorna a lista resultante de acrescentar à primeira lista os elementos da segunda que nao ocorrem na primeira.
 
unionRec ::  Eq a => [a] -> [a]-> [a]
unionRec l [] = l 
unionRec l (h:t) = if elem h l then unionRec l t else h : unionRec l t 


-- 24 Retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda.

intersectRec ::  Eq a => [a] ->[a] -> [a]
intersectRec [] l = []
intersectRec l [] = l 
intersectRec (x:xs) l = if elem x l then x : intersectRec xs l else intersectRec xs l  

-- 25 Dado  um  elemento  e  uma  lista  ordenada  retorna  a  lista  resultante  de  inserir ordenadamente esse elemento na lista.

insertRec ::  Ord a => a -> [a]-> [a]
insertRec x [] = [x]
insertRec n (h:t) | n>h = h:insertRec n t 
                  | otherwise = n:h:t 

-- 26 Junta todas as strings da lista numa só, separando-as por um espaço.

unwordsRec ::  [String] -> String
unwordsRec [x] = x
unwordsRec (h:t) = h ++ " " ++ unwordsRec t

-- 27 Junta todas as strings da lista numa só, separando-as pelo caracter '\n'.

unlinesRec ::  [String] -> String
--unlinesRec [x] = x    --SE NAO QUISESSE QUE ACABASSE EM \n TINHA QUE POR ISTO EM VEZ DE unlinesRec [] = []
unlinesRec [] = []
unlinesRec (h:t) = h ++ "\n" ++ unlinesRec t 

-- 28 Dada uma lista nao vazia, retorna a posĩçao onde se encontra o maior elemento da lista.  As posiçoesda lista comecam em 0, i.e., a funçao devera retornar 0 se o primeiro elemento da lista for o maior.

pMaiorRec ::  Ord a => [a] -> Int
pMaiorRec l = pMaiorRecAux2 (pMaiorRecAux l) l 0

pMaiorRecAux :: Ord a => [a] -> a  -- Esta funçao retorna o maior elemento de uma lista 
pMaiorRecAux [x] = x
pMaiorRecAux (x:y:z) = if x>y then pMaiorRecAux (x:z) else pMaiorRecAux (y:z)

pMaiorRecAux2 :: Eq a => a -> [a] -> Int -> Int -- o int é a posiçao
pMaiorRecAux2 n [] x = x
pMaiorRecAux2 n (h:t) x = if n==h then x else pMaiorRecAux2 n t (x+1)


-- 29 Testa se uma lista tem elementos repetidos.

temRepetidosRec ::  Eq a => [a] -> Bool
temRepetidosRec [] = False 
temRepetidosRec (x:xs) = if elem x xs then True else temRepetidosRec xs  

-- 30 Determina a lista dos algarismos de uma dada lista de caracteres.

algarismosRec ::  [Char] -> [Char] 
algarismosRec [] = []
algarismosRec (h:t) = if elem h ['0'..'9'] then h:algarismosRec t 
                      else algarismosRec t 

algarismosRecMinha :: [Char] -> [Char]
algarismosRecMinha [] = []
algarismosRecMinha (h:t) = if h>='0' && h<= '9' then h:algarismosRecMinha t 
                           else algarismosRecMinha t

-- 31 Determina os elementos de uma lista que ocorrem em posiçoes  ́ımpares.  Considere que o primeiro elemento da lista ocorre na posiçao 0 e por isso par

posImparesRec ::  [a] -> [a]
posImparesRec [] = []
posImparesRec (x:y:zs)= y : posImparesRec zs 

-- 32 Determina os elementos de uma lista que ocorrem em posições ímpares. Considere que o primeiro elemento da lista ocorre na posição 0 e por isso é par

posParesRec :: [a] -> [a]
posParesRec [] = []
posParesRec (x:y:z) = x : posParesRec z 

-- 33 Testa se uma lista está ordenada por ordem crescente.

isSortedRec ::  Ord a => [a] -> Bool
isSortedRec [x] = True
isSortedRec (x:y:z) = if x<=y then isSortedRec (y:z) else False  

-- 34 Calcula o resultado de ordenar uma lista.

iSortRec ::  Ord a => [a] -> [a]
iSortRec [] = []
iSortRec (h:t) = insertRec h (iSortRec t) 

-- 35 Dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo a ordem lexicográfica.

menorRec :: String -> String -> Bool
menorRec _ "" = False
menorRec "" _ = True 
menorRec (x:xs) (y:ys) = x<y || menorRec xs ys  

-- 36 Testa se um elemento pertence a um multi-conjunto. Nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero.

elemMSetRec ::  Eq a => a -> [(a,Int)] -> Bool    
elemMSetRec x [] = False                           
elemMSetRec x ((a,b):xs) = if x==a then True else elemMSetRec x xs

-- 37 Calcula o tamanho de um multi-conjunto atraves da soma no segundo termo de cada elemento.

lengthMSetRec :: [(a,Int)] -> Int
lengthMSetRec [] = 0
lengthMSetRec ((a,b):t) = b + lengthMSetRec t 

-- 38 Converte um multi-conjuto na lista dos seus elementos.

converteMSetRec ::  [(a,Int)] -> [a]
converteMSetRec [] = [] 
converteMSetRec ((a,b):t) = replicate b a ++ converteMSetRec t 

-- 39 Acrescenta um elemento a um multi-conjunto.

insereMSetRec ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSetRec x [] = [(x,1)]
insereMSetRec x ((a,b):t) | x==a = ((a,b+1):t)
                          | otherwise = (a,b) : insereMSetRec x t 

-- 40 Remove um elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o multi-conjunto recebido.

removeMSetRec ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSetRec x [] = []
removeMSetRec x ((a,b):t) | x==a = t 
                          | otherwise = (a,b):removeMSetRec x t

--ESTA FUNCAO SO REMOVE O ELEMENTO SE SO EXISTIR 1 UNIDADE DESSE ELEMENTO (ex: (c,1) é removido enquanto que (c,2) ficaria (c,1))
--removeMSetRec x ((a,b):t) | x==a && b==1 = t 
--                          | x==a && b>1 = (a,b-1):t 
--                          | otherwise = (a,b):removeMSetRec x t

-- 41 Dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos.

constroiMSetRec ::  Ord a => [a] -> [(a,Int)]

constroiMSetRec [] = []
constroiMSetRec (h:t) = insereMSetRec h (constroiMSetRec t)
 
--esta auxiliar ja nao serve para nada porque a outra nao dava certo 
constroiMSetRec2Aux :: Ord a => a -> [a] -> Int -- Diz quantas vezes um numero aparece na lista
constroiMSetRec2Aux _ [] = 0
constroiMSetRec2Aux n (h:t) | n==h = 1 + constroiMSetRec2Aux n t  
                           | otherwise = constroiMSetRec2Aux n t


-- 42 Divide uma lista de Either s em duas listas.

partitionEithersAula :: [Either a b] -> ([a],[b])
partitionEithersAula [] = ([],[])
partitionEithersAula (Left x : t) = (x:la, lb)
                                  where  (la,lb) = partitionEithersAula t 
partitionEithersAula (Right x : t) = (la,x:lb) 
                                   where (la,lb) = partitionEithersAula t 

-- OUTRO MODO (que utiliza uma auxiliar) : 
partitionEithersRec ::  [Either a b] -> ([a],[b])
partitionEithersRec [] = ([],[])
partitionEithersRec (Left a:t) = (<+>) ([a],[]) (partitionEithersRec t)
partitionEithersRec (Right b:t) = (<+>) ([],[b]) (partitionEithersRec t)

(<+>) :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
(<+>) (x1,x2) (y1,y2) = (x1++y1,x2++y2) 


-- 43 Coleciona os elementos do tipo a de uma lista

catMaybesRec :: [Maybe a] -> [a]
catMaybesRec [] = []
catMaybesRec (Just a : t) = a : catMaybesRec t
catMaybesRec (Nothing : t) = catMaybesRec t

-- 44  Dada uma posicao  inicial  (coordenadas)  e  uma  lista  de  movimentos,  calcula  a  posicao  final  do  robot depois de efectuar essa sequencia de movimentos.

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,y+1) t 
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t 

-- 45 Dadas as posices inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o robot passe de uma posicao para a outra.

caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1>x2 = Oeste : caminho (x1-1,y1) (x2,y2)
                        | x1<x2 = Este : caminho (x1+1,y1) (x2,y2)
                        | y1>y2 = Sul : caminho (x1,y1-1) (x2,y2)
                        | y1<y2 = Norte : caminho (x1,y1+1) (x2,y2)
                        |otherwise = []

-- 46 Testa se uma lista de movimentos so e composta por movimentos verticais (Norte ou Sul).

vertical ::  [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t 
vertical (h:t) = False

-- 47 Dada uma lista nao vazia de posicoes, determina a que esta mais perto da origem (note que as coordenadas de cada ponto sao numeros inteiros)

data Posicao = Pos Int Int deriving Show

maisCentral ::  [Posicao] -> Posicao
maisCentral [x] = x
--maisCentral ((Pos x1 y1):(Pos x2 y2):z) = if (x1<x2 && y1<y2) then maisCentral ((Pos x1 y1):z)  else maisCentral ((Pos x2 y2):z)  
maisCentral ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral ((Pos x y):ps) else maisCentral ((Pos a b):ps)

-- 48 Dada uma posicao e uma lista de posicoes, selecciona da lista as posiçoes adjacentes a posiçao dada.

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1):t) | x==x1+1 = (Pos x1 y1):vizinhos (Pos x y) t 
                                   | x==x1-1 = (Pos x1 y1):vizinhos (Pos x y) t 
                                   | y==y1+1 = (Pos x1 y1):vizinhos (Pos x y) t
                                   | y==y1-1 = (Pos x1 y1):vizinhos (Pos x y) t 
                                   | otherwise = vizinhos (Pos x y) t 

-- 49  Testa se todas as posiçoes de uma dada lista tem a mesma ordenada. 

mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2): t) = if x1 == x2 then mesmaOrdenada ((Pos x2 y2):t) else False 

-- 50 Testa se o estado dos semaforos de um cruzamento ́e seguro, i.e., nao ha mais do que semaforo nao vermelho (so pode haver um verde ou um amarelo)

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = False
interseccaoOK l = interseccaoPermitida l <= 1 

interseccaoPermitida :: [Semaforo] -> Int 
interseccaoPermitida [] = 0
interseccaoPermitida (Vermelho:t) = interseccaoPermitida t 
interseccaoPermitida (_:t) = 1+interseccaoPermitida t 
 

 
 



