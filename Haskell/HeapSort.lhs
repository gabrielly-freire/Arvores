> module HeapSort where  

> import Prelude hiding ( (!!) )
> import Data.List hiding ( (!!) )

Primeiro, façamos uma versão safe p/ 
a (!!). 

> (!!) :: [a] -> Integer -> Maybe a 
> [] !! _ = Nothing
> (x : xs) !! n
>   | n == 0    = Just x 
>   | n > 0     = xs !! (n - 1)
>   | otherwise = Nothing


> len :: [a] -> Integer
> len []       = 0
> len (_ : xs) = 1 + len xs

O padrão para a Heap será 
em termos da HeapMax. Para acessar
a HeapMin, partindo da default, apenas 
use a [flip (<=)].

2n + 1, 2n + 2
(xs !! n) >= (xs !! (2n + 1)) 
&& (xs !! n) >= (xs !! (2n + 2))

> (|>=) :: Ord a => Maybe a -> Maybe a -> Bool
> Just x  |>= Just y = x >= y
> Nothing |>= Just _ = False
> _ |>= _            = True 


> test1 :: Ord a => [a] -> Integer -> Bool
> test1 xs n = (xs !! n) |>= (xs !! ((2 * n) + 1)) 

> test2 :: Ord a => [a] -> Integer -> Bool
> test2 xs n = (xs !! n) |>= (xs !! ((2 * n) + 2)) 

> isHeap :: Ord a => [a] -> Bool  
> isHeap xs = and bs' 
>   where bs' = [test1 xs n && test2 xs n | n <- [0 .. len xs]]

> countdown :: Integer -> [Integer]
> countdown x 
>   | x < 0  = []
>   | x == 0 = [x] 
>   | x > 0  = x : countdown (x - 1)








