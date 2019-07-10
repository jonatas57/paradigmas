module Main where

import Data.List

votos :: [String]
votos = ["Vermelho", "Azul", "Verde", "Azul", "Azul", "Vermelho"]

votosRank :: [[String]]
votosRank = [["Vermelho","Verde"],
            ["Azul"],
            ["Verde","Vermelho","Azul"],
            ["Azul","Verde","Vermelho"],
            ["Verde"]]

main :: IO ()
main = do
  print $ vencedor  votos
  print $ vencedor' votosRank

-- conta quantos votos o candidato x recebeu
conta :: Eq a => a -> [a] -> Int
conta e xs = sum [1 | x <- xs, e == x]


-- retorna a lista de elementos unicos
unicos :: Eq a =>[a]->[a]
unicos (x:xs) = unicos' (x:xs) []
  where
    unicos' [] ys                 = ys
    unicos' (x:xs) ys | elem x ys = unicos' xs ys
                      | otherwise = unicos' xs (ys ++ [x])

-- retorna uma lista de pares ordenados (votos, candidato) com o total
-- de votos obtido por cada candidato use a função sort para ordenar
-- do menos para o mais votado
resultado :: Ord a => [a] -> [(Int, a)]
resultado xs = sort [(conta x xs, x) | x <- unicos xs]

-- retorna o vencedor da eleição
vencedor :: Ord a => [a] -> a
vencedor xs = snd $ last (resultado xs)

-- elimina as listas vazias de uma lista de listas
rmvazio :: Eq a => [[a]] -> [[a]]
rmvazio [] = []
rmvazio xss = [xs | xs <- xss, not $ null xs]

-- elimina um candidato da lista de votos
elimina :: Eq a => a -> [[a]] -> [[a]]
elimina e xss = [elimina' e xs | xs <- xss]
  where
    elimina' _ [] = []
    elimina' e xs = [x | x <- xs, x /= e]

-- retorna uma lista dos candidatos existentes, do menos para o mais votado
rank :: Ord a => [[a]] -> [a]
rank xss = [snd xs | xs <- resultado (first xss)]
  where
    first xss = [y | (y:ys) <- xss]

-- retorna o vencedor executando o processo descrito acima
vencedor' :: Ord a => [[a]] -> a
vencedor' xss | length ranked == 1 = head ranked
              | otherwise          = vencedor' (rmvazio (elimina (head ranked) xss))
  where
    ranked = unicos (rank xss)
