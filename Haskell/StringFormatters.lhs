Formatador de Strings 

> module StringFormatters where 

> removeFirstComma :: String -> String 
> removeFirstComma (',' : str) = str 
> removeFirstComma str       = str 

> removeDuplicatedCommas :: String -> String 
> removeDuplicatedCommas ""                = "" 
> removeDuplicatedCommas [c]               = [c]
> removeDuplicatedCommas (c : c' : cs) = 
>   case (c, c') of
>     (',', ',') -> cs' 
>     _          -> c : cs' 
>   where cs' = removeDuplicatedCommas (c' : cs)

> (!=) :: Eq a => a -> a -> Bool
> (!=) = curry (not . uncurry (==))

> removeUnderscores :: String -> String
> removeUnderscores = filter (!= '_')

> formatTreeStr :: String -> String
> formatTreeStr = removeFirstComma . removeDuplicatedCommas 










