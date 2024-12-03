Vou tentar uma abordagem sem ponteiros 
e nem hashs, mantendo simples, 
ainda que menos eficiente. 

\begin{code}
  module Trees where 
  import qualified Data.Tree as T
  import StringFormatters
  import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
 -- import Data.IORef
 -- import Control.Monad.IO.Class (liftIO) 
\end{code}
     

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }


Certo, vamos começar.
Primeiro, definamos as árvores
de maneira generalista p/ α. 

\begin{code}
  data Tree a where
   Nil  :: Tree a 
   Tree :: { val      :: a
           , leftSon  :: Tree a
           , rightSon :: Tree a
           }          -> Tree a
    deriving (Eq)  

  toGeneralTree :: Tree a -> T.Tree a
  toGeneralTree Nil            = error "Void Tree" 
  toGeneralTree (Tree val l r) = 
    case (l, r) of 
      (Nil, Nil)               -> T.Node val []

      (Tree {}, Nil) -> T.Node val [toGeneralTree l]

      (Nil, Tree {}) -> T.Node val [toGeneralTree r]

      (Tree {},
       Tree {})       -> T.Node val [toGeneralTree l,
                                     toGeneralTree r]
      
    


  instance Show a => Show (Tree a) where
    show :: Show a => Tree a -> String
    show tree =  show $ toGeneralTree tree
  
  p = pretty 
  pretty tree = putStrLn $ T.drawTree $ toGeneralTree tree


  -- Definicao útil
  leaf :: a -> Tree a 
  leaf x = Tree x Nil Nil


  timed :: (a -> b) -> a -> IO (b, NominalDiffTime)
  timed f x =
    do 
      start <- getCurrentTime
      let result = f x
      end <- getCurrentTime
      let duration = diffUTCTime end start
      return (result, duration)
    
\end{code}


Talvez essa sintaxe ainda não pareça muito amigável, mas é questão de costume
Agora vamos definir como exibir listas (`traduzí-las` em String) de uma maneira padrão. 

Vamos adicionar umas árvores padrão para testes:

\begin{code}
  t0 = Nil
  t1 = Tree 1 t0 t0 
  t2 = Tree 2 t0 t1 
  t3 = Tree 3 t1 t2 
  t4 = Tree 4 t2 t3 
  t5 = Tree 5 t3 t4

  -- Os exemplos padrão usados em slide:
  tA :: Tree String 
  tA = Tree "A" tB tC 

  tB :: Tree String 
  tB = Tree { val      = "B"
            , leftSon  = Tree "D" Nil (Tree "J" Nil Nil)
            , rightSon = Tree "H" Nil Nil
            }

  tC :: Tree String 
  tC = Tree { val      = "C"
            , leftSon  = Tree "E" Nil Nil
            , rightSon = Tree "F" (Tree "I" Nil Nil) Nil
            }

\end{code}

Vamos definir algumas funções auxiliares:

\begin{code}
  height :: Integral i => Tree a -> i
  height Nil          = 0 
  height (Tree _ l r) = 
   1 + max (height l) (height r)

  projectFloor :: Integral i => i -> Tree a -> [a]
  projectFloor _ Nil              = []
  projectFloor num (Tree val tl tr)  
    | num < 0  = error "negative Floor"
    | num == 0 = [val]
    | num > 0  = projectFloor (num - 1) tl 
              ++ projectFloor (num - 1) tr
    

  showFloor :: (Integral i, Show a) => i -> Tree a -> String
  showFloor 0 Nil = "_"
  showFloor _ Nil = ""
  showFloor num tree@(Tree val l r) 
    | num < 0  = error "num < 0" 
    | num == 0 = show val
    | num > 0  = showFloor (num - 1) l ++ "," ++ showFloor (num - 1) r

\end{code}

Agora podemos definir o caminho em Nível: 

\begin{code}
  inPreOrder :: Tree a -> [a]
  inPreOrder Nil            = []
  inPreOrder (Tree val l r) =
    [val] ++ inPreOrder l ++ inPreOrder r

  inSimOrder :: Tree a -> [a]
  inSimOrder Nil            = [] 
  inSimOrder (Tree val l r) = 
    inSimOrder l ++ [val] ++ inSimOrder r


  inPosOrder :: Tree a -> [a]
  inPosOrder Nil            = [] 
  inPosOrder (Tree val l r) = 
    inPosOrder l ++ inPosOrder r ++ [val]


  inLevel :: Tree a -> [a]
  inLevel tree = concat [projectFloor n tree | n <- [0 .. height tree - 1]]
\end{code}

Árvores de busca binária:

\begin{code}
  isBinarySearch :: Ord a => Tree a -> Bool
  isBinarySearch Nil            = True 
  isBinarySearch (Tree rootVal l r) = 
    case (l, r) of 
      (Nil, Nil)                 -> True 
      (Nil, Tree rightVal l2 r2) -> rootVal < rightVal

                                 && isBinarySearch l2 
                                 && isBinarySearch r2

      (Tree leftVal  l1 r1, Nil) -> leftVal < rootVal

                                 && isBinarySearch l1
                                 && isBinarySearch r1
      (Tree leftVal  l1 r1, 
       Tree rightVal l2 r2)      -> leftVal < rootVal
                                 && rootVal < rightVal
                                 
                                 && isBinarySearch l2 
                                 && isBinarySearch r2
                                
                                 && isBinarySearch l1
                                 && isBinarySearch r1
\end{code}


Uns exemplos pra testes: 

\begin{code}
  exampleTree20 :: Integral i => Tree i
  exampleTree20 =
    Tree { val      = 20
         , leftSon  = exampleTree10
         , rightSon = exampleTree40
         }    

  exampleTree10 :: Integral i => Tree i
  exampleTree10 = 
    Tree { val      = 10 
         , leftSon  = Nil
         , rightSon = leaf 15
         }    

  exampleTree40 :: Integral i => Tree i
  exampleTree40 = 
    Tree { val      = 40
         , leftSon  = leaf 30
         , rightSon = Tree 50 Nil (leaf 60) 
         }    

\end{code}


Algoritmo de Busca: 

\begin{code}
  isIn :: Ord a => a -> Tree a -> Bool
  isIn x Nil = False
  isIn x (Tree val l r) = 
    x == val || x `isIn` l || x `isIn` r
\end{code}





\begin{code}
  showFormattedFloor :: (Integral i, Show a) => i -> Tree a -> String
  showFormattedFloor num tree = formatTreeStr (showFloor num tree)
    
    {-
      case (l,r) of 
         (Nil, Nil) -> "" 
         (Nil, r)   -> showFloor (num - 1) r 
         (l,  Nil)  -> showFloor (num - 1) l 
         (l,  r)    -> showFloor (num - 1) l ++ ", " ++ showFloor (num - 1) r  
    -}
  


  showHeight :: (Show a, Integral i) => Tree a -> i -> [String]
  showHeight Nil _ = [] 
  showHeight tree@(Tree val leftSon rightSon) num
    | num < 0  = error "Numero < 0"
    | num == 0 = [show val]
    | num == 1 = showHeight leftSon (num - 1)  ++ showHeight rightSon (num - 1)
    | num > 1  = showHeight tree (num - 1)
     
    
\end{code}


\begin{code} 
{-
  instance Show (Tree a) where 
    show :: Tree a -> String
    show Nil = "Nil"
    show (Tree val leftSon rightSon) = 
      case leftSon of 
        Nil  -> ""
        tree -> ""
-}
\end{code}

  Esse show funfará p/ Árvores pequenas


\begin{code}
  getVal :: Tree a -> Maybe a
  getVal Nil = Nothing
  getVal Tree { val } = Just val
  
  getLeftSon :: Tree a -> Tree a
  getLeftSon Nil = Nil
  getLeftSon Tree { leftSon } = leftSon
  
  getRightSon :: Tree a -> Tree a
  getRightSon Nil = Nil
  getRightSon Tree { rightSon } = rightSon
\end{code}





/begin{code}
   
/end{code}








