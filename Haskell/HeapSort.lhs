> module HeapSort where  

> import Prelude hiding ( (!!) )
> import Data.List hiding ( (!!) )
> import Util

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

> sonsOf :: Ord a => [a] -> Integer -> (Maybe a, Maybe a)
> sonsOf xs n = (xs !! ((2 * n) + 1), xs !! ((2 * n) + 2))

> test1 :: Ord a => [a] -> Integer -> Bool
> test1 xs n = (xs !! n) |>= (xs !! ((2 * n) + 1)) 

> test2 :: Ord a => [a] -> Integer -> Bool
> test2 xs n = (xs !! n) |>= (xs !! ((2 * n) + 2)) 

> isHeap :: Ord a => [a] -> Bool  
> isHeap xs   = and bs' 
>   where bs' = [greaterThanSons xs n | n <- [0 .. len xs - 1]]
>         greaterThanSons xs n = uncurry (&&)                          $
>                                cross ((xs !! n) |>=) ((xs !! n) |>=) $
>                                sonsOf xs n
    

> countdown :: Integer -> [Integer]
> countdown x 
>   | x < 0  = []
>   | x == 0 = [x] 
>   | x > 0  = x : countdown (x - 1)








