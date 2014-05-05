import Data.List
import Data.Char

-- 'interact' makes the rest of the arguments take input from stdin and prints out the result to stdout
main = interact $ show . compute

compute str = computesum $ sort $ tokenize ',' $ filter (\x -> x /= '\"') str

-- Tokenizes a string using the input separator
tokenize sep str =  let
                        split = splitUpto (\x -> x == sep) str
                    in
                        if null (snd split) then [(fst split)]
                        else [(fst split)] ++ tokenize sep (snd split)

-- Splits the list lst into a pair at the position where f is true
splitUpto f lst = let
                    splitUptoHelper res lst =   if null lst then (res,[]) else if f (head lst) then (res, tail lst)
                                                else splitUptoHelper (res ++ [head lst]) (tail lst)
                  in
                    splitUptoHelper [] lst

computesum lst = sum $ zipWith (\a b -> a*b) (map wordsum lst) [1,2..]

-- Computes sum of letters in a word
wordsum = sum . map (\x -> 1 + ord x - ord 'A')

