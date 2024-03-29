module Cesar where

import Data.Char

let2Int :: Char -> Int
let2Int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7,  7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4,  0.2, 2.0, 0.1]

-- retorna a n-esima letra seguinte,
-- evite ultrapassar o limite com `mod` 26
shift :: Int -> Char -> Char
shift x c | isLower c  = int2let (mod (let2Int c + x) 26)
          | otherwise  = c

-- aplica a função shift em cada letra da string
encode :: Int -> String -> String
encode n s = [shift n c | c <- s]

decode :: Int -> String -> String
decode n s = encode (-n) s

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

-- quantidade de letras minúsculas
lowers :: String -> Int
lowers []     = 0
lowers (x:xs) = (if isLower x then 1 else 0) + lowers xs

-- conta a ocorrência de um caracter em uma String
count :: Char -> String -> Int
count _ []     = 0
count c (x:xs) = (if x == c then 1 else 0) + count c xs

-- dado um n e m, calcule 100 * n / m
percent :: Int -> Int -> Float
percent n m = 100.0 * ((fromIntegral n) / (fromIntegral m))

-- calcule a porcentagem de cada letra minúscula
-- do alfabeto em uma String
-- a porcentagem é a contagem de ocorrência pelo total
-- de letras minúsculas
freqs :: String -> [Float]
freqs s = [percent (count c s) (lowers s) | c <- s]

-- Calcule a medida de Chi-Quadrado de duas
-- tabelas de frequência
-- Soma (Observado - Esperado) ^ 2 / Esperado
chisqr :: [Float] -> [Float] -> Float
chisqr obs esp = sum [(a - b) ^ 2 / b | (a, b) <- zip obs esp]

-- rotaciona uma tabela em n posicoes
rotate :: Int -> [a] -> [a]
rotate 0 l      = l
rotate n (x:xs) = rotate (n - 1) (xs ++ [x])

-- retorna a lista de posições que contém um
-- elemento x
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (y, i) <- zip xs [0..], y == x]
