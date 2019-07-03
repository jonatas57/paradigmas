module Test where

import Cesar

-- aplicando shift duas vezes, uma com valor negativo, o caracter
-- deve ser o mesmo
prop_neg_shift :: Int -> Char -> Bool
prop_neg_shift n c = (encode n (encode (-n) [c])) == [c]

-- o tamanho da mensagem codificada deve ser o mesmo da original
prop_enc_length :: Int -> String -> Bool
prop_enc_length n s = length (encode n s) == length s

-- o decode do encode deve ser a string original
prop_enc_dec :: Int -> String -> Bool
prop_enc_dec n s = decode n (encode n s) == s
