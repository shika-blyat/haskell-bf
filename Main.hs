module Main where

import Data.Char



enumerate l = zip [0..] l

setListElemTo :: [Int] -> Int -> Int -> [Int]
setListElemTo l n new = [ if k == n then new else i | (k, i) <- (enumerate l)]


evalBf :: [Int] -> Int -> Int -> [Char] -> [Int] -> IO [Int]

evalBf loop_index ptr codeptr s l
        | length s <= codeptr = do
            return l
        | first == '+' = evalBf loop_index ptr (codeptr + 1) s ([ if k == ptr then i + 1 else i | (k, i) <- (enumerate l)])
        | first == '-' = evalBf loop_index ptr (codeptr + 1) s ([ if k == ptr then i - 1 else i | (k, i) <- (enumerate l)])
        | first == '>' = evalBf loop_index (ptr + 1) (codeptr + 1) s l
        | first == '<' = evalBf loop_index (ptr - 1) (codeptr + 1) s l
        | first == '.' = do
            print $ chr $ l !! ptr
            evalBf loop_index ptr (codeptr + 1) s l
        | first == ',' =
        do
            input <- getLine
            evalBf loop_index ptr (codeptr + 1) s (setListElemTo l ptr (ord (input !! 0)))
        | first == '[' = evalBf (loop_index ++ [codeptr + 1]) ptr (codeptr + 1) s l
        | first == ']' = if l !! ptr == 0
            then evalBf (init loop_index) ptr (codeptr + 1) s l
            else evalBf loop_index ptr (last loop_index) s l
        | otherwise = do
            return l
        where first = s !! codeptr

main :: IO ()

main =  do
    result <- evalBf [0] 0 0 ">+++++++>+>+><[<]>[[>]<<[->>+>+<<<]>>>[<<<+>>>-]<<[->>+>+<<<]>>>[<<<+>>>-]<[-<+>]<[<]>-] " [0 | k <- [0..20]]
    print result
